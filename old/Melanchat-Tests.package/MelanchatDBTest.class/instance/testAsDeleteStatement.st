deleting
testAsDeleteStatement
	self
		assert:
			(db
				parameters: (Dictionary with: #id -> 23);
				table: 'users';
				asDeleteStatement) sql
		equals: 'delete from users where id = @id'.
	"uses id if present"
	self
		assert:
			(db
				parameters: (Dictionary with: #id -> 23 with: #name -> 'name');
				table: 'users';
				asDeleteStatement) sql
		equals: 'delete from users where id = @id'.
	"with MelanchatDB DBNull"
	self
		assert:
			(db
				parameters: (Dictionary with: #name -> db class DBNull);
				table: 'users';
				asDeleteStatement) sql
		equals: 'delete from users where name is null'.
	"default behavior is and if no id present"
	self
		assert:
			(db
				parameters: (Dictionary with: #gender -> 'F' with: #name -> 'name');
				table: 'users';
				asDeleteStatement) sql
		equals: 'delete from users where name = @name and gender = @gender'