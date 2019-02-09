initialization
setUp
|user|
super setUp.
	user := UsersDB new.
	
	user copyFrom: dummyUser.
	
	dummyUser := user.