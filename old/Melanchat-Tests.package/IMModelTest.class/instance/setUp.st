initialization
setUp
	super setUp.
	dummyUser insert.
	model := IMModel new id: dummyUser id