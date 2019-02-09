utilities
testResultSet
	dummyUser insert.
	dummyUser insert.
	self assert: (db class resultSet: Dictionary new) isEmpty description: 'Anything other than a PGResult should return an empty collection.'.
		self assert: (db class resultSet: nil) isEmpty description: 'Anything other than a PGResult should return an empty collection.'.
	self assert: (db rawQuery: 'select 23 from users') equals: (OrderedCollection with: 23 with: 23).
	self assert: (db rawQuery: 'select * from users') first isDictionary description: 'The array should contain dictionaries.'