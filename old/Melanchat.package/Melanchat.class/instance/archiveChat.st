im-messages
archiveChat
"I am the route to archive a chat."
	^ self class runJSONRequest: [ :messages | messages archiveChat ] withModel: MessagesModel