selecting
selectForLoggedin
	"I select an user hiding sensitive columns."

	^ self db
		select:
			(QueryParameters query: 'select' , self fieldsForLoggedin , 'from users u where id = @id' parameters: (Users new id: id))
		into: self model