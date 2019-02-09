selecting
testSelectContacts
	| userID anotherUserID yetAnotherUserID messageID |
	userID := dummyUser insert.
	anotherUserID := dummyUser insert.
	yetAnotherUserID := dummyUser insert.
	Histories new
		sender: userID;
		recipient: anotherUserID;
		date: DateAndTime now;
		insert.
	Messages new
		sender: anotherUserID;
		recipient: userID;
		date: DateAndTime now;
		content: 'sup';
		insert.
	Messages new
		sender: userID;
		status: 2;
		recipient: anotherUserID;
		date: DateAndTime now;
		content: 'sup!';
		insert.
	Histories new
		sender: anotherUserID;
		recipient: yetAnotherUserID;
		date: DateAndTime now;
		insert.
	Messages new
		sender: anotherUserID;
		recipient: yetAnotherUserID;
		date: DateAndTime now;
		content: 'sup!';
		insert.
	messageID := Messages new
		sender: yetAnotherUserID;
		recipient: anotherUserID;
		date: DateAndTime now;
		content: 'sup!!';
		insert.
	self assert: ((table sender: userID) selectContacts allSatisfy: [ :contact | contact class = IMModel ]) description: 'should return a list of IMModel'.
	self
		assert:
			((table sender: userID) selectContacts
				allSatisfy: [ :contact | contact user avatar isNotNil and: [ contact user tags isEmpty and: [ contact user languages isEmpty ] ] ])
		description: 'user instvar should be formatted for display'.
	self assert: ((table sender: userID) selectContacts collect: [ :im | im user id ]) equals: (OrderedCollection with: anotherUserID).
	self assert: ((table sender: anotherUserID) selectContacts collect: [ :im | im user id ]) equals: (OrderedCollection with: userID with: yetAnotherUserID).
	self assert: (table sender: anotherUserID) selectContacts second history second id equals: messageID