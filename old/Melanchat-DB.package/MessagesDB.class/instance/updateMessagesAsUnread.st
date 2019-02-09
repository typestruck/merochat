updating
updateMessagesAsUnread
	self db update: (QueryParameters query: 'update messages set status = 1 where recipient = @sender and sender = @recipient and status = 2' parameters: self)