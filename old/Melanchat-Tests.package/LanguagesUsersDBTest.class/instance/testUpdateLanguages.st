updating
testUpdateLanguages
	| id |
	id := UsersDB new
		name: 'test';
		gender: 'O';
		birthday: '1900-05-09';
		password: 'password';
		email: 'email@email.com';
		headline: 'chocolate';
		description: 'please';
		insert.
	table
		language: 1;
		speaker: id;
		insert.
			table updateLanguages: #(2 3 4).
			"deleting existing prior to update"
				self assert: (db count: (LanguagesUsers new speaker: id ; language: 1)) equals: 0.
	"updating"

	self assert: (db count: (LanguagesUsers new speaker: id)) equals: 3
