updating
asUpdateStatement
	| parametersCopy |
	parametersCopy := parameters copy.
	parametersCopy removeKey: #id ifAbsent: [].
	sql := 'update ' , table , ' set ' , (self class as: parametersCopy comparingList: ', ' isNull: false)
		, ' where id = @id'