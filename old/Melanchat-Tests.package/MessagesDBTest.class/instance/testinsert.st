inserting
testinsert
	| userID |
	userID := dummyUser insert.
	dummyUser insert.
	MessagesDB new
		sender: userID;
		recipient: dummyUser id;
		date: DateAndTime now;
		content: 'sup';
		insert.
	self assert: (db count: Histories new) equals: 1.
	MessagesDB new
		sender: dummyUser id;
		recipient: userID;
		date: DateAndTime now;
		content: 'sup!';
		insert.
	self assert: (db count: Histories new) equals: 1.
	self
		assert:
			(db
				count:
					(Messages new
						sender: dummyUser id;
						recipient: userID))
		equals: 1.
	self
		assert:
			(db
				count:
					(Messages new
						sender: userID;
						recipient: dummyUser id))
		equals: 1