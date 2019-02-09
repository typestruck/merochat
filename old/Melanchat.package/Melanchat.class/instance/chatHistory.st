im-messages
chatHistory
"I am the route for getting the chat history between the logged in user and a recipient."
	^ self class runJSONRequest: [ :model | model chatHistory ] withModel: MessagesModel 