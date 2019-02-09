updating
testUpdateTags
	| userID tagID  |
	userID := UsersDB new
		name: 'test';
		gender: 'O';
		birthday: '1900-05-09';
		password: 'password';
		email: 'email@email.com';
		headline: 'chocolate';
		description: 'please';
		insert.
	tagID := Tags new
		name: 'tag';
		insert.
	 Tags new
		name: 'tag2';
		insert.
	table
		tag: tagID;
		creator: userID;
		insert.
	table updateTags: #('tag2').
	"deleting existing prior to update"
	self
		assert:
			(db
				count:
					(TagsUsers new
						tag: tagID;
						creator: userID))
		equals: 0.
	"updating"
	self assert: (db count: (TagsUsers new creator: userID)) equals: 1