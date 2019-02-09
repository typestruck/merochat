selecting
selectByEmail
	^ self db
		select:
			(QueryParameters query: 'select * from users where email = lower(@email)' parameters: (Users new email: email))
		into: self model