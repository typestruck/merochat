deleting
delete
	"I create a delete statement. If id is present in the dictionary it is used in the where clause; otherwise all keys in the dictionary are used."

self asDeleteStatement .
	^ self execute .