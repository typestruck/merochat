requests
idFromRequest: request
	"I extract an user id from a json web token. GET requests should have it on cookies, and POST on the x-access-token header."

	request method = #GET
		ifTrue: [ ^ self
				idFromGETToken:
					(request cookies
						detect: [ :cookie | cookie name = 'melanchat' ]
						ifFound: [ :cookie | cookie value ]
						ifNone: [ '' ]) ]
		ifFalse: [ ^ self idFromPOSTToken: (request headers at: 'x-access-token' ifAbsent: '') ]