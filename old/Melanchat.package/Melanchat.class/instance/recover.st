user-recover
recover
	"I am the route for recovering an user's password."

	^ self class runJSONRequest: [ :user | user recover ] withModel: UsersModel