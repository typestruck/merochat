updating
archiveChat
	self db withTransaction: [ :transDB |
		transDB rawQuery:
			(QueryParameters
				query:
					'update histories set senderArchived = true where sender = @sender and recipient = @recipient;'
				parameters: self).
		transDB
		rawQuery:
			(QueryParameters
				query:
					'update histories set recipientArchived = true where sender = @recipient and recipient = @sender'
				parameters: self)
	]
			