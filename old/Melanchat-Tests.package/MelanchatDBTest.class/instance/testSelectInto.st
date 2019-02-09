selecting
testSelectInto
	| id otherID result user |
	"into new object"
	id := dummyUser insert.
	result := db select: (dummyUser id: id) asTableClass into: Users.
	self assert: result class equals: Users.
	self assert: result id equals: id.
	"into existing object"
	otherID := dummyUser insert.
	user := UsersModel new avatar: 'avatar.png'.
	db select: (Users new id: otherID) into: user.
	self assert: user id equals: otherID.
	self assert: user avatar equals: 'avatar.png'.
	"raw query into collection"
	self assert: (db select: (QueryParameters query: 'select * from users') into: {Users}) isCollection.
	self assert: (db select: (QueryParameters query: 'select * from users where id = 9080980980') into: Users) isNil description: 'no results for object projection must return nil'.
	self assert: (db select: (QueryParameters query: 'select * from users where id = 9080980980') into: {Users}) isEmpty description: 'no results for collection projection must return empty collection'.
	"using string"
	self assert: (db select: 'select * from users' into: {Users}) isCollection