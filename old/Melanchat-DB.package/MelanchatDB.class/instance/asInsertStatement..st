inserting
asInsertStatement: conflict
	"I am an insert statement, optionally using a conflict instruction, and returning the inserted id."

	parameters removeKey: #id ifAbsent: [  ].
	sql := 'insert into ' , table , '(' , self asColumnList , ') values(' , self asValueList , ')'
		, (conflict ifNotEmpty: [ ' ' , conflict ]) , ' returning id'