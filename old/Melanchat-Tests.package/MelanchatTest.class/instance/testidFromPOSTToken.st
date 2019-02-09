requests
testidFromPOSTToken
	| association |
	association := (Melanchat createToken: 23) asDictionary.
	self assert: (Melanchat idFromPOSTToken: '') equals: nil.
	self assert: (Melanchat idFromPOSTToken: '2343') equals: nil.
	"only token2 can be used for post requests"
	self assert: (Melanchat idFromPOSTToken: (association at: #tokenGET)) equals: nil.
	self assert: (Melanchat idFromPOSTToken: (association at: #tokenPOST)) equals: 23