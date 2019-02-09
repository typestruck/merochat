recover
testRecover
	| newModel |
	self should: [ model recover ] raise: BadRequest.
	model insert.
	newModel := UsersModel new email: model email.
	newModel recover.
	self assert: newModel name equals: model name.
	self assert: (db count: (Recoveries new recoverer: model id)) equals: 1