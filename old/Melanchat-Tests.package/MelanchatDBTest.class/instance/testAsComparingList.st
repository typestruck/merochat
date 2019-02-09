utilities
testAsComparingList
	self
		assert:
			(db
				parameters: (Dictionary with: #id -> 23);
				asComparingList: ', ')
		equals: 'id = @id'.
	self
		assert:
			(db
				parameters:
					{(#id -> 23).
					(#name -> 'test')} asDictionary;
				asComparingList: ' and ')
		equals: 'name = @name and id = @id'.
	self
		assert:
			(db
				parameters:
					{(#id -> 23).
					(#name -> db class DBNull)} asDictionary;
				asComparingList: ' or ')
		equals: 'name is null or id = @id'