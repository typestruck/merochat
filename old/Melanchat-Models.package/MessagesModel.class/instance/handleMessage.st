messages
handleMessage
	"I process a message come through a socket."

	self token
		ifNotNil: [
			 "new message"
			type = 4
				ifTrue: [ sender := self token.
					^ self newMessage ] ]
		ifNil: [ BadRequest signal: 'invalid id' ].
	^ nil