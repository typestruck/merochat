login
testLogin
	| password newModel |
	password := model password.
	model
		password: (model class hashPassword: model password);
		insert.
	newModel := UsersModel new.
	self
		assert: [ newModel
				email: '';
				password: '';
				login: password ]
		badRequest: 'Invalid email or password'.
		
	self assert: (model login: password ) isDictionary 