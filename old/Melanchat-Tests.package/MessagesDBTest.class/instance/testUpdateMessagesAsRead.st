updating
testUpdateMessagesAsRead
	| userID |
	userID := dummyUser insert.
	dummyUser insert.
	MessagesDB new
		sender: userID;
		recipient: dummyUser id;
		date: DateAndTime now;
		content: 'sup';
		insert.
	MessagesDB new
		sender: userID;
		recipient: dummyUser id;
		date: DateAndTime now;
		content: 'sup';
		insert.
	MessagesDB new
		sender: dummyUser id;
		recipient: userID;
		date: DateAndTime now;
		content: 'sup!';
		insert.
	table
		sender: userID;
		recipient: dummyUser id;
		updateMessagesAsRead.
	self
		assert:
			(table selectChat
				allSatisfy:
					[ :message | (message sender = userID and: [ message status = 2 ]) or: [ message sender = dummyUser id and: [ message status = 1 ] ] ])
		description: 'should have all status as 2'