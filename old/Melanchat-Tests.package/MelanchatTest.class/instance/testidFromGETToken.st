requests
testidFromGETToken
	| association |
	association := (Melanchat createToken: 23) asDictionary.
	self assert: (Melanchat idFromGETToken: '') equals: nil.
	self assert: (Melanchat idFromGETToken: '2343') equals: nil.
	"only token1 can be used for get requests"
	self assert: (Melanchat idFromGETToken: (association at: #tokenPOST)) equals: nil.
	self assert: (Melanchat idFromGETToken: (association at: #tokenGET)) equals: 23
	