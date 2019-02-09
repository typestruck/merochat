selecting
testSelectSuggestions
	| userID |
	dummyUser insert.
	dummyUser insert.
	userID := dummyUser insert.
	dummyUser insert.
	MessagesDB new
		sender: userID;
		recipient: dummyUser id;
		content: 'sup';
		insert.
	self
		assert: (dummyUser selectSuggestions allSatisfy: [ :e | e id ~= dummyUser id ])
		description: 'must not be self user'.
	self
		assert: (dummyUser selectSuggestions allSatisfy: [ :e | e id ~= userID ])
		description: 'must not be someone previsouly talked to'