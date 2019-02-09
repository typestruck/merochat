db
testQueryBy
	db query: 'select' by: [  ].	"strings as in rawQuery"
	self assert: db sql equals: 'select'.
	self assert: db parameters keys isEmpty description: 'parameters should never be nil'.
	"using QueryParameters"
	db query: (QueryParameters query: 'select * from users where id = @id' parameters: (Users new id: 23)) by: [  ].
	self assert: db sql equals: 'select * from users where id = @id'.
	self assert: (db parameters at: #id) equals: 23.
	"using table classes"
	db query: (Users new id: 23) by: [  ].
	self assert: db sql isNil description: 'should be set by block'.
	self assert: (db parameters at: #id) equals: 23.
	"using collections"
	db
		query:
			{(Users new id: 23).
			(Users new id: 24)}
		by: [  ].
	self assert: db sql isNil description: 'should be set by block'.
	self assert: (db parameters allSatisfy: [ :p | p isDictionary ]) description: 'parameters should be a dictionary collection'.
	self assert: (db parameters second at: #id) equals: 24