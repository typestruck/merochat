user
display
	"I am the route for getting an user's public information."

	^ self class
		runJSONRequest: [ :request | 
			UsersModel new
				id: (request at: #id);
				formatForDisplay: nil ]