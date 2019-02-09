inserting
insert
	"I call the function insertHistory, which takes cares of not duplication histories."

	^ self db rawQuery: (QueryParameters query: 'select insertHistory(@sender, @recipient)' parameters: self)