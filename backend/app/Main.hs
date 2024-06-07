{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as C
import Data.Text
import Data.Text.Read (decimal)
import Data.Time
import qualified Data.Vector as V
import Database.SQLite.Simple
import Network.HTTP.Types
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Web.Scotty

data Task = Task Text Text Text Bool (Maybe Int) (Maybe UTCTime)

instance ToJSON Task where
  toJSON (Task name ninja category checked taskId timestamp) =
    object
      [ "name" .= name,
        "ninja" .= ninja,
        "category" .= category,
        "checked" .= checked,
        "id" .= taskId,
        "timestamp" .= timestamp
      ]

instance FromJSON Task where
  parseJSON = withObject "Task" $ \v ->
    Task
      <$> v .: "name"
      <*> v .: "ninja"
      <*> v .: "category"
      <*> v .: "checked"
      <*> v .:? "id"
      <*> v .:? "timestamp"

instance FromRow Task where
  fromRow = Task <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Task where
  toRow (Task name ninja category_ checked _ _) = toRow (name, ninja, category_, fromEnum checked)

instance Show Task where
  show task = show $ encode task

newtype TaskList = TaskList [Task]

instance FromJSON TaskList where
  parseJSON = withObject "TaskList" $ \v ->
    TaskList
      <$> v .: "tasks"

instance ToJSON TaskList where
  toJSON (TaskList taskList) = object ["tasks" .= taskList]

instance Show TaskList where
  show x = show $ encode x

unTaskList :: TaskList -> [Task]
unTaskList (TaskList x) = x

newtype AppError = AppError Text

instance ToJSON AppError where
  toJSON (AppError err) = object ["error" .= err]

newtype NinjaName = NinjaName Text deriving (Eq)

instance ToJSON NinjaName where
  toJSON (NinjaName x) = String x

instance FromRow NinjaName where
  fromRow = NinjaName <$> field

newtype NameList = NameList [NinjaName]

instance ToJSON NameList where
  toJSON (NameList x) = object ["names" .= x]

maybeToRight :: b -> Maybe a -> Either b a
maybeToRight _ (Just x) = Right x
maybeToRight y Nothing = Left y

startDatabase :: IO Connection
startDatabase = do
  conn <- open "test.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS tasks (id INTEGER PRIMARY KEY, task TEXT, ninja TEXT, category TEXT, checked INTEGER NOT NULL DEFAULT 0 CHECK(checked IN (0, 1)), timestamp DATETIME)"
  return conn

userTasks :: Connection -> Text -> IO TaskList
userTasks conn user = do
  r <- query conn "SELECT task, ninja, category, checked, id, timestamp FROM tasks WHERE ninja=?" (Only user) :: IO [Task]
  return $ TaskList r

taskById :: Connection -> Int -> IO (Either Text Task)
taskById conn taskId = do
  r <- query conn "SELECT task, ninja, category, checked, id, timestamp FROM tasks WHERE id=?" (Only taskId) :: IO [Task]
  return $ maybeToRight "Couldn't find task given id." (returnFirst r)
  where
    returnFirst (x : _) = Just x
    returnFirst _ = Nothing

flipTask :: Connection -> Int -> Either Text Bool -> IO (Either Text Task)
flipTask conn taskId (Right checked) = do
  task <- taskById conn taskId
  case task of
    Right _ -> do
      execute conn "UPDATE tasks SET checked = ? WHERE id = ?" [fromEnum checked, taskId]
      taskById conn taskId
    _ -> return task
flipTask _ _ (Left x) = return $ Left x

textToBool :: Either String (Int, Text) -> Either Text Bool
textToBool (Right (0, _)) = Right False
textToBool (Right (1, _)) = Right True
textToBool (Right (_, _)) = Left "Input not 0 or 1"
textToBool (Left x) = Left $ pack x

dateToUTC :: String -> Either Text UTCTime
dateToUTC date = maybeToRight "Couldn't parse date!" $ parseTimeM True defaultTimeLocale "%Y-%m-%d" date

setTaskDate :: Connection -> Int -> Either Text UTCTime -> IO (Either Text Task)
setTaskDate conn taskId (Right date) = do
  beforeTask <- taskById conn taskId
  case beforeTask of
    Left _ -> return beforeTask
    Right _ -> do
      execute conn "UPDATE tasks SET timestamp = ? WHERE id = ?" (date, taskId)
      taskById conn taskId
setTaskDate _ _ (Left x) = return $ Left x

createTask :: Connection -> Task -> IO ()
createTask conn = execute conn "INSERT INTO tasks (task, ninja, category, checked) VALUES (?,?,?,?)"

createUndoneTask :: Connection -> Text -> Text -> Text -> IO ()
createUndoneTask conn task ninja category = createTask conn (Task task ninja category False Nothing Nothing)

getNames :: Connection -> IO NameList
getNames conn = do
  r <- query_ conn "SELECT DISTINCT ninja FROM tasks" :: IO [NinjaName]
  return $ NameList r

createUser :: Connection -> Text -> IO AppError
createUser conn ninja = do
  (NameList names) <- getNames conn
  if NinjaName ninja `notElem` names
    then do
      csvData <- BL.readFile "assignments.csv"
      case C.decode C.HasHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \(name, category) ->
          createUndoneTask conn name ninja category
      return $ AppError "Success!"
    else return $ AppError "User already exists!"

deleteTask :: Connection -> Int -> IO ()
deleteTask conn taskId = execute conn "DELETE FROM tasks WHERE id = ?" (Only taskId)

runServer :: IO ()
runServer = do
  conn <- startDatabase
  scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "static")
    middleware simpleCors
    middleware logStdoutDev
    get "/" $ file "./static/index.html"
    get "/users/:name" $ do
      name <- pathParam "name"
      tasks <- liftIO $ userTasks conn name
      json tasks
    post "/users/:name" $ do
      name <- pathParam "name"
      databaseResponse <- liftIO $ createUser conn name
      json databaseResponse
    post "/tasks/:taskId" $ do
      taskId <- pathParam "taskId"
      myParams <- queryParams
      case myParams of
        [("input", input)] -> do
          taskBool <- liftIO $ flipTask conn taskId (textToBool $ decimal input)
          case taskBool of
            Left x -> json $ AppError x
            Right x -> json x
        [("date", date)] -> do
          taskDate <- liftIO $ setTaskDate conn taskId (dateToUTC $ unpack date)
          case taskDate of
            Left x -> json $ AppError x
            Right x -> json x
        _ -> json $ AppError "Either query parameter isn't working!"
    get "/tasks/:id" $ do
      taskId <- pathParam "id"
      task <- liftIO $ taskById conn taskId
      case task of
        Left x -> do
          status status404
          json $ AppError x
        Right x -> json x
    delete "/tasks/:id" $ do
      taskId <- pathParam "id"
      liftIO $ deleteTask conn taskId
    get "/names" $ do
      names <- liftIO $ getNames conn
      json names
  close conn

main :: IO ()
main = runServer
