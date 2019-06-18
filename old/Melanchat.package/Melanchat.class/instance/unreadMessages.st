im-messages
unreadMessages
"I am the route to set all last messages as unread."
	^ self class runJSONRequest: [ :messages | messages unreadMessages ] withModel: MessagesModel