im
testChattingWith
	| userID |
	dummyUser insert.
	userID := dummyUser insert.
	dummyUser insert.
	MessagesDB new
		sender: userID ;
		recipient: model id;
		date: DateAndTime now;
		content: 'sup';
		insert.
	model suggestions: model suggest.
	model chattingWith: nil.
	"should be the first suggestion when not talking to a specific user "
	self assert: model chatting equals: model suggestions first.
	model chattingWith: userID.
	self assert: model chatting user id equals: userID.
	self assert: model chatting history size equals: 1