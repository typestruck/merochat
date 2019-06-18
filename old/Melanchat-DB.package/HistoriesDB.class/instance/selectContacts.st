selecting
selectContacts
	"I select the users which have messages with the given id. If there are any unread messages, the chat history is selected as well."

	| users |
	users := self db
		rawQuery:
			(QueryParameters
				query:
					'(select recipientArchived archived, ' , UsersDB fieldsForDisplay , ' from users u join histories h on u.id = h.sender and h.recipient = @sender) union all (select senderArchived archived, '
						, UsersDB fieldsForDisplay , ' from users u join histories h on u.id = h.recipient and h.sender = @sender)'
				parameters: self).
	"lol select n + 1"
	^ users
		collect: [ :archiveUser | 
			| user |
			user := MelanchatMapper setInstVars: UsersModel from: archiveUser.
			IMModel new
				user: (user forDisplay);
				archived: (archiveUser at: #archived);
				history:
					(MessagesDB new
						recipient: (archiveUser at: #id);
						sender: sender;
						selectChat) ]