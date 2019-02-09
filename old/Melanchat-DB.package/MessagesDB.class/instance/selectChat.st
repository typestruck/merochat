selecting
selectChat
"we prolly gonna need to load only n messages at a time"
	^ self db
		select:
			(QueryParameters
				query:
					(self class
						orderHistory:
							'select ' , self class fieldsMessageDisplay
								, ' from messages where sender in (@sender, @recipient) and recipient in (@sender, @recipient)')
				parameters: self)
		into: {Messages}