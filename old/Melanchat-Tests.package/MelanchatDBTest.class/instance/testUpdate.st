updating
testUpdate
	| id newName |
	newName := 'update test'.
	id := db insert: dummyUser asTableClass.
	db update: (dummyUser id:id;name: newName) asTableClass.
	self assert: (db select: (Users new id: id) into: Users) name equals: newName.

	"using raw queries"
	newName := 'hot cake'.
	db delete: (QueryParameters query: 'update users set name = @name where id = @id' parameters: (Dictionary with: #id -> id with: #name -> newName)).
	self assert: (db select: (Users new id: id) into: Users) name equals: newName