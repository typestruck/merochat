copying
testAsTableClass
	self assert: UsersModel new asTableClass class equals: Users.
	self assert: (UsersModel new name: dummyUser name) asTableClass name equals: dummyUser name