updating
testUpdate
	| id |
	id := dummyUser insert.
	dummyUser
		id: id;
		name: 'updated';
		update.
	db update: dummyUser asTableClass .
	self assert: (db select: (Users new id: id) into: Users) name equals: 'updated'