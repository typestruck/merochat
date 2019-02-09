inserting
testInsert
	| userID |
	userID := db insert: dummyUser asTableClass.
	self assert: userID isNumber.
	self assert: ((db select: (Users new id: userID) into: Users) name) equals: 'test'