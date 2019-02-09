requests
testIdFromRequest
	| association get post |
	association := (Melanchat createToken: 23) asDictionary.
	get := self class emptyGET.
	self assert: (Melanchat idFromRequest: get) equals: nil.
	get setCookie: 'melanchat=' , (association at: #tokenGET).
	self assert: (Melanchat idFromRequest: get) equals: 23.
	post := self class emptyPOST.
	self assert: (Melanchat idFromRequest: post) equals: nil.
	post headers at: 'x-access-token' put: (association at: #tokenPOST).
	self assert: (Melanchat idFromRequest: post) equals: 23.

	"check if token2 is not accepted for get requests"
	get := self class emptyGET.
	get setCookie: 'melanchat=' , (association at: #tokenPOST).
	self assert: (Melanchat idFromRequest: get) equals: nil.
	"check if token1 is not accepted for post requests"
	post := self class emptyPOST.
	post headers at: 'x-access-token' put: (association at: #tokenGET).
	self assert: (Melanchat idFromRequest: post) equals: nil