messages
testHandleMessage
	| response |
	model recipient: dummyUser insert.
	model type: 3.
	response := model handleMessage.
	self assert: response isNil description: 'type 3 should only update messages'.
	model type: 4.
	model content: 'sup'.
	response := model handleMessage.
	self assert: response isDictionary description: 'type 4 should return a dictionary'.
	"sender reply must be type 2"
	self assert: (response at: #sender) type  equals: 2.
		"recipient reply must be type 4"
	self assert: (response at: #recipient) type  equals: 4
