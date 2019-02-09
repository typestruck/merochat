deleting
asDeleteStatement
	"I am a delete statement. If id is present in the dictionary, I use it in the where clause; otherwise all keys in the dictionary are used."

	sql := 'delete from ' , table , ' where '
		,
			((parameters includesKey: #id)
				ifTrue: [ 'id = @id' ]
				ifFalse: [ self asComparingList: ' and ' ])