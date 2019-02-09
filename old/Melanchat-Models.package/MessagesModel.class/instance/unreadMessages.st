messages
unreadMessages
	"I update all messages between sender and recipient as unread."

	MessagesDB new
		sender: sender;
		recipient: recipient;
		updateMessagesAsUnread.
	^ Melanchat ok