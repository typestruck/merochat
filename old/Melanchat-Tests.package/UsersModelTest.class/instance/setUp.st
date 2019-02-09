initialization
setUp
	super setUp.
	model := UsersModel new.
model copyFrom: dummyUser .