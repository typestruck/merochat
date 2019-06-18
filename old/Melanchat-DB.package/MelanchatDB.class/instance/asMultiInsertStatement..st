inserting
asMultiInsertStatement: conflict
		"I am a multivalued insert statement, optionally using a conflict instruction, and returning the inserted id."

	sql := 'insert into ' , table , '(' , (self class asColumnList: parameters first) , ') values'
		,
			((', '
				join:
					(parameters collectWithIndex: [ :dictionary :index | '(' , (self class asValueList: dictionary withSuffix: index asString) , ')' ]))
				, (conflict ifNotEmpty: [ ' ' , conflict ]) , ' returning id')