selecting
selectSuggestions
	"I select users to chat with."

	^ (self db
		select:
			(QueryParameters
				query: 'select' , self class fieldsForDisplay , 'from users u where id <> @id and not exists(select 1 from histories where sender in (@id, u.id) and recipient in (@id, u.id)) order by u.id desc'
				parameters: (Users new id: id))
		into: {UsersModel})
		collectWithIndex: [ :cuser :index | 
			IMModel new
				user: (cuser formatForDisplay: cuser id);
				history: {} ]