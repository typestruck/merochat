settings
updateSettings
"I update non public information of an user."
	(email isEmptyOrNil or: [ password isEmptyOrNil ])
		ifTrue: [ BadRequest signal: 'Invalid email or password' ].
	UsersDB new
		email: email;
		id: id;
		password: (self class hashPassword: password);
		update.
	^ Melanchat ok