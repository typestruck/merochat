login
login: attemptedPassword
"I login an user."
	(UsersDB model: self)
		email: email;
		selectByEmail.
	(password isNil
		or: [ (PBKDF2 derivedKeySHA1Password: attemptedPassword salt: MelanchatConfiguration salt) hex ~= password ])
		ifTrue: [ BadRequest signal: 'Invalid email or password' ].
	^ Melanchat createToken: id