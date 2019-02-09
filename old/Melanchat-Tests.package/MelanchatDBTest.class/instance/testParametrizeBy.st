utilities
testParametrizeBy
	self assert: (db parameters: nil ; sql: 'select * from users' ; parametrize ) equals: 'select * from users'.
	self assert: (db parameters: Dictionary new ;  sql: 'select * from users' ; parametrize ) equals: 'select * from users'.
	self
		assert: (db parameters:  (Dictionary with: #id -> 23); sql:'select * from users where id = @id' ;parametrize )
		equals: 'select * from users where id = $1'.
	"nil values are ignored"
	self
		assert:
			(db parameters:	{(#age -> nil).
					(#name -> 'test').
					(#id -> 34)} asDictionary;
				 sql:'update users set name = @name where id = @id';
				parametrize
				)
		equals: 'update users set name = $1 where id = $2'.
	"MelanchatDB null is used for null"
	self
		assert:
			(db parameters:{(#name ->  db class DBNull).
					(#id -> 34)} asDictionary;
				sql:'update users set name = @name where id = @id';
				parametrize
					)
		equals: 'update users set name = null where id = $2'.
	"parameters with no binding should be left as it is"
	self
		assert: (db parameters: (Dictionary with: #id -> 23);  sql:'select * from users where id = @id or id = @aid' ;parametrize)
		equals: 'select * from users where id = $1 or id = @aid'.
	"parameter names must match"
	self
		assert:
			(db parameters:	{(#id -> 23).
					(#aid -> 345)} asDictionary;
				 sql:'select * from users where id = @id or id = @aid';
				parametrize
				)
		equals: 'select * from users where id = $2 or id = $1'.
	"parameter order in the query must be respected"
	self
		assert:
			(db parameters:	{(#id -> 23).
					(#aid -> 345)} asDictionary;
				sql:'select * from users where id = @aid or id = @id or id = @aid';
				parametrize
				)
		equals: 'select * from users where id = $1 or id = $2 or id = $1'