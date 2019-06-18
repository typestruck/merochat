user-signup
signup
	"I am the route for registering a new user."

	^ self class
		runJSONRequest: [ :user :request | user sign: (request at: #captchaResponse) up: (request headers at: ZnConstants remoteAddressHeader) ]
		withModel: UsersModel