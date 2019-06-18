messages
readMessages
	"I update all messages between sender and recipient as read."

	MessagesDB new
		sender: recipient;
		recipient: sender;
		updateMessagesAsRead.
	^ Melanchat ok