updating
updateMessagesAsRead
	self db
		update:
			(QueryParameters
				query: 'update messages set status = 2 where recipient = @recipient and sender = @sender and status = 1'
				parameters: self)