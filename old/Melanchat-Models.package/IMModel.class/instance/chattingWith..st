im
chattingWith: chattingID
	"I set chatting with the first suggestion if chattinID is nil, or the chattingID user otherwise."

	chatting := chattingID
		ifNil: [ suggestions ifEmpty: [ nil ] ifNotEmpty: [ suggestions first ] ]
		ifNotNil: [ 
			self class new
				user:
					(UsersModel new
						id: chattingID ;
						formatForDisplay: id);
				history:
					(MessagesModel new
						sender: id;
						recipient:chattingID;
						chatHistory) ]