selecting
testSelectChat
	| userID |
	userID := dummyUser insert.
	dummyUser insert.
	Messages new
		sender: userID;
		recipient: dummyUser id;
		date: DateAndTime now;
		content: 'sup';
		insert.
	Messages new
		sender: dummyUser id;
		recipient: userID;
		date: DateAndTime now;
		content: 'sup!';
		insert.
	self assert: ((table sender: userID; recipient: dummyUser id) selectChat allSatisfy: [ :message | message class = Messages ]) description: 'must return a collection of Messages'