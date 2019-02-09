user-login
login
"I am the route for logging an user in."
	^ self class runJSONRequest: [ :user :request | user login: (request at: #password) ] withModel: UsersModel