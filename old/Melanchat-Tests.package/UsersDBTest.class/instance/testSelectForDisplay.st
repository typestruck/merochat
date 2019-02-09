selecting
testSelectForDisplay
	| newUser |
	dummyUser insert.
	newUser := (UsersDB new id: dummyUser id) selectForDisplay.
	self assert: newUser password isNil