selecting
asCountStatement
	sql := 'select count(1) from ' , table .
	
	parameters keys ifNotEmpty: [ sql := sql , ' where ' , (self asComparingList: ' and ')]