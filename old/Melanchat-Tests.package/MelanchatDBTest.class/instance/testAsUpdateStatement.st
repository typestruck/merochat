updating
testAsUpdateStatement
	self
		assert:
			(db parameters: 
				
					{(#id -> 23).
					(#name -> 'name').
					(#gender -> db class DBNull)} asDictionary;
					table: 'users';
				asUpdateStatement ) sql
		equals: 'update users set name = @name, gender = null where id = @id'