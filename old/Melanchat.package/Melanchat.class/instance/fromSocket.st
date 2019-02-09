im
fromSocket
	"most likely we gonna need some sort of lock or syncronization for imSockets"

	^ ZnWebSocketDelegate
		map: '/im/socket'
		to: [ :socket | 
			| sender |
			[ socket
				runWith: [ :rawMessage | 
					| message |
					message := MelanchatMapper setInstVars: MessagesModel from: (NeoJSONReader fromString: rawMessage).
					sender := message token.
					sender ifNotNil: [ imSockets at: sender ifAbsentPut: [ socket ] ].
					[
					message handleMessage
						ifNotNil: [ :reply | 
							(imSockets at: message recipient ifAbsent: [ nil ])
								ifNotNil: [ :receivingSocket | receivingSocket sendMessage: (self class asJSON: (reply at: #recipient)) ].
							socket sendMessage: (self class asJSON: (reply at: #sender)) ] ]
						on: BadRequest
						do: [ socket sendMessage: (self class asJSON: (Dictionary with: #type -> 0)).
							socket close ] ] ]
				on: ConnectionClosed , PrimitiveFailed
				do: [ imSockets removeKey: sender ifAbsent: [  ] ] ]