user-settings
updateProfile
"I am the route for updating an user's profile, i.e., their publicaly available information."
	^ self class runJSONRequest: [ :user | user updateProfile ] withModel: UsersModel