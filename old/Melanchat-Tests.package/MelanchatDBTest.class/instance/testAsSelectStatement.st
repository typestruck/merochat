selecting
testAsSelectStatement
	self
		assert:
			(db
				parameters:
					{(#id -> 23).
					(#name -> 'name').
					(#gender -> db class DBNull)} asDictionary;
				table: 'users';
				asSelectStatement) sql
		equals: 'select * from users where name = @name and id = @id and gender is null'