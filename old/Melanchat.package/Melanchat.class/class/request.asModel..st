requests
request: request asModel: class
	"I return an object of the given model class, setting instance variables matching with request parameters. If class = UsersModel or IMModel, the json web token present in the request is automatically decoded into id, and into sender if MessagesModel."

	| model |
	model := class new.
	class allInstVarNames do: [ :var | model instVarNamed: var put: (request at: var ifAbsent: [ nil ]) ].
	(class = UsersModel or: [ class = IMModel ])
		ifTrue: [ model id: (self idFromRequest: request) ].
	class = MessagesModel
		ifTrue: [ model sender: (self idFromRequest: request)  ].
	^ model