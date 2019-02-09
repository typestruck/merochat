selecting
selectForDisplay
	"I select an user hiding sensitive columns."

	^ self db
		select:
			(QueryParameters
				query: 'select' , self class fieldsForDisplay , 'from users u where id = @id'
				parameters: (Users  new id: id))
		into: self model