im-messages
readMessages
	"I am the route to set all last messages as read."

	^ self class runJSONRequest: [ :messages | messages readMessages ] withModel: MessagesModel