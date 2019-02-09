messages
testChatHistory
	| userID anotherUserID messages |
	userID := dummyUser insert.
	anotherUserID := dummyUser insert.
	MessagesDB new
		sender: userID;
		recipient: model sender;
		date: DateAndTime now;
		content: 'sup';
		insert.
	MessagesDB new
		sender: userID;
		recipient: anotherUserID;
		date: DateAndTime now;
		content: 'sup';
		insert.
	messages := model
		recipient: userID;
		chatHistory.
	self assert: messages size = 1 description: 'only one message was inserted for this id'.
	"sender must not be encoded"
	self assert: (messages first sender) equals: userID