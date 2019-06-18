accessing
token
	^ token isString ifTrue: [ Melanchat  idFromPOSTToken: (  token) ] ifFalse: [ token ]