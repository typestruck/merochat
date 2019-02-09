initialization
setUp
	super setUp.
	dummyUser insert.
	model := MessagesModel new sender: dummyUser id; token: ((Melanchat createToken:dummyUser  id) at: #tokenPOST) .