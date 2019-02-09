utilities
createToken: id
	"I return a dictionary with two json web tokens. Key #tokenGET is meant to be used for GET requests, whereas #tokenPOST is meant for POST requests, as a measure against exploits like CSRF."

	| jws |
	jws := JsonWebSignature new
		algorithmName: 'HS256';
		payload: ((JWTClaimsSet new at: 'user' put: id) expirationTime: 3471300000).
	jws key: MelanchatConfiguration tokenSecret.
	^ Dictionary
		with: #tokenGET -> jws compactSerialized
		with: #tokenPOST -> (jws key: MelanchatConfiguration  tokenSecret2) compactSerialized