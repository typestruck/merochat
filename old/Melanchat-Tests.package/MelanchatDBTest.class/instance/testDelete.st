deleting
testDelete
	| id |
	"id present"

	id := dummyUser insert.
	db delete: (dummyUser id: id) asTableClass.
	self assert: ((db rawQuery: 'select count(1) from users') first at: #count) equals: 0.
	"default behavior when id is not present"
	dummyUser insert.
	db
		delete:
			(Users new
				name: dummyUser name;
				gender: dummyUser gender).
	self assert: ((db rawQuery: 'select count(1) from users' )first at: #count) equals: 0.
	"using raw queries"
	id := dummyUser insert.
	db delete: (QueryParameters query: 'delete from users where id = @id' parameters: (Dictionary with: #id -> id)).
	self assert: ((db rawQuery: 'select count(1) from users') first at: #count) equals: 0