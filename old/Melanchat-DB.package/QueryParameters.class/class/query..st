initialization
query: sql 
	^ self new
		query: sql;
		parameters: Dictionary new