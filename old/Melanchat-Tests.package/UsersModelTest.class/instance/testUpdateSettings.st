settings
testUpdateSettings
	model insert.
	self should: [ (model copy email: nil) updateSettings ] raise: BadRequest.
	self should: [ (model copy password: nil) updateSettings ] raise: BadRequest.

	model
		email: model email , '.nz';
		password: model password , model password;
		updateSettings.
		
	self assert: (db select: (Users new id: model id) into: Users) password equals: (UsersModel hashPassword: model password)