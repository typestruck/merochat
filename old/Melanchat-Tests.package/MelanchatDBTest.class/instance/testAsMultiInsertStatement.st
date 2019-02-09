inserting
testAsMultiInsertStatement
	self
		assert:
			(db
				parameters:
					{{(#gender -> 'F').
					(#name -> 'test')} asDictionary.
					{(#gender -> db class DBNull).
					(#name -> 'test2')} asDictionary};
				table: 'users';
				asMultiInsertStatement: '') sql
		equals: 'insert into users(name, gender) values(@name1, @gender1), (@name2, null) returning id'.
			self
		assert:
			(db
				parameters:
					{{(#gender -> 'F').
					(#name -> 'test')} asDictionary.
					{(#gender -> db class DBNull).
					(#name -> 'test2')} asDictionary};
				table: 'users';
				asMultiInsertStatement: 'on conflict(email) do nothing') sql
		equals: 'insert into users(name, gender) values(@name1, @gender1), (@name2, null) on conflict(email) do nothing returning id'.
