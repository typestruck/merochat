user-settings
updateSettings
"I am the route for updating an user's settings, i.e., not publicaly available information like email or password."
	^ self class runJSONRequest: [ :user | user updateSettings ] withModel: UsersModel