import { useState, useEffect } from "preact/hooks"
import { render } from "preact";

function UserCreator(props) {
	const [username, setUserName] = useState("");
	const [buttonShowing, setButtonShowing] = useState(true);

	function onSubmit(e) {
		if (username) {
			let uri = 'http://localhost:3000/users/' + username
			fetch(encodeURI(uri), { method: "POST" })
			if (props.onSubmit)
				props.onSubmit(username);

			setUserName("");
			setButtonShowing(false);
			setTimeout(() => {
				setButtonShowing(true);
			}, 500);
			e.preventDefault();
		}
	}

	function onChange(e) {
		setUserName(e.currentTarget.value)
	}

	return (
		<div>
			<h3>Create User:</h3>
			<form onSubmit={onSubmit}>
				<input type="text" value={username} onInput={onChange} />
				{buttonShowing && <button type="submit">Create</button>}
			</form>
		</div>
	)
}

function Task(props) {
	const [checked, setChecked] = useState(false);
	const [name, setName] = useState("");

	useEffect(() => {
		fetch('http://localhost:3000/tasks/' + props.id)
			.then(response => response.json())
			.then(task => {
				setChecked(task.checked)
				setName(task.name)
			});
	}, [props.id]);

	function toggle(_) {
		let checkedNum = (!checked) ? 1 : 0;
		fetch('http://localhost:3000/tasks/' + props.id + '?input=' + checkedNum, { method: "POST" })
			.then(response => response.json())
			.then(task => {
				setChecked(task.checked)
			});
	}

	return (
		<li id={props.id}>
			<label>
				<input type="checkbox" checked={checked} onClick={toggle} />{name}
			</label>
		</li>
	);
}

function Category(props) {
	const [tasks, setTasks] = useState([]);
	const [opened, setOpened] = useState(false);

	useEffect(() => {
		setTasks(props.tasks);
	}, [opened, props.tasks])

	function onToggle() {
		setOpened(!opened);
	}

	return (
		<div>
			<details onClick={onToggle} >
				<summary>{props.name}</summary>
				<ul>
					{tasks.map(task =>
						<Task id={task.id} />
					)}
				</ul>
			</details>
		</div>
	);
}

function groupBy(list, keyGetter) {
	const map = new Map();
	list.forEach((item) => {
		const key = keyGetter(item);
		const collection = map.get(key);
		if (!collection) {
			map.set(key, [item]);
		} else {
			collection.push(item);
		}
	});
	return map;
}

function TaskList(props) {
	const [tasks, setTasks] = useState(new Map);

	useEffect(() => {
		if (props.name != "") {
			fetch('http://localhost:3000/users/' + props.name)
				.then(response => response.json())
				.then(taskList => {
					setTasks(groupBy(taskList.tasks, ({ category }) => category))
				});
		} else {
			setTasks(new Map);
		}
	}, [props.name]);

	return (
		<div>
			{[...tasks.keys()].map((category) =>
				<Category name={category} tasks={tasks.get(category)} />
			)}
		</div>
	);
}

function Names(props) {
	const [currentName, setCurrentName] = useState("");
	const [nameList, setNameList] = useState({ names: [] });

	useEffect(() => {
		fetch('http://localhost:3000/names')
			.then(response => response.json())
			.then(names => {
				setNameList(names);
				setCurrentName(props.currentName);
			});
	}, [props.currentName]);

	useEffect(() => {
		fetch('http://localhost:3000/names')
			.then(response => response.json())
			.then(names => {
				setNameList(names)
				if (names.names.length > 0) {
					setCurrentName(names.names[0]);
					props.onChange(names.names[0])
				} else {
					setCurrentName("");
				}
			});
	}, []);

	function onChange(e) {
		setCurrentName(e.currentTarget.value)
		if (props.onChange)
			props.onChange(e.currentTarget.value)
	}

	if (!nameList) return <h1>Loading...</h1>

	return (
		<div>
			<select name="names" value={currentName} onChange={onChange}>
				{nameList.names.map(name =>
					<option value={name}>{name}</option>
				)}
			</select>
		</div>
	);
}

function App(_) {
	const [currentName, setCurrentName] = useState("");

	function onChange(name) {
		setCurrentName(name);
	}

	function onSubmit(name) {
		setTimeout(function() {
			setCurrentName(name);
		}, 700)
	}
	return (
		<div>
			<UserCreator onSubmit={onSubmit} />
			<Names onChange={onChange} currentName={currentName} />
			<TaskList name={currentName} />
		</div>
	);
}

render(<App />, document.getElementById('app'));
