user-recover
reset
	"I am the route for reseting an user's password."

	^ self class runJSONRequest: [ :user :request | user reset: (request at: #uuid ifAbsent: '') ] withModel: UsersModel