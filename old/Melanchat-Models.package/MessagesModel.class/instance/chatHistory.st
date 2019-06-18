messages
chatHistory
	"I select the chat history between two users."

	^ MessagesDB new
		sender: self sender;
		recipient: self recipient;
		selectChat