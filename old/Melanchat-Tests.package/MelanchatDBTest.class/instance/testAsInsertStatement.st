inserting
testAsInsertStatement
	self
		assert:
			(db
				parameters:
					{(#gender -> 'F').
					(#name -> 'test')} asDictionary;
				table: 'users';
				asInsertStatement:'') sql
		equals: 'insert into users(name, gender) values(@name, @gender) returning id'.
	self
		assert:
			(db
				parameters:
					{(#gender -> 'F').
					(#name -> 'test')} asDictionary;
				table: 'users';
				asInsertStatement:'on conflict(email) do nothing') sql
		equals: 'insert into users(name, gender) values(@name, @gender) on conflict(email) do nothing returning id'.
	"ignorinig nil"
	self
		assert:
			(db
				parameters:
					{(#gender -> 'F').
					(#name -> nil)} asDictionary;
				table: 'users';
				asInsertStatement:'') sql
		equals: 'insert into users(gender) values(@gender) returning id'.
	"MelanchatDB DBNull for null"
	self
		assert:
			(db
				parameters:
					{(#gender -> 'F').
					(#name -> db class DBNull)} asDictionary;
				table: 'users';
				asInsertStatement:'') sql
		equals: 'insert into users(name, gender) values(null, @gender) returning id'