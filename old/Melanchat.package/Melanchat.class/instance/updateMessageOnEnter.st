user-settings
updateMessageOnEnter
"I am the route for updating whether enter submits a message or not."
	^ self class runJSONRequest: [ :user | user updateMessageOnEnter ] withModel: UsersModel