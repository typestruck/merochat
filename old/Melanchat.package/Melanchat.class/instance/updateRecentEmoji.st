user-settings
updateRecentEmoji
	"I am the route for updating the list of recently used emojis."

	^ self class runJSONRequest: [ :user | user updateRecentEmoji ] withModel: UsersModel