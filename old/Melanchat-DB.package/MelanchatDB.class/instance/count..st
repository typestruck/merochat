selecting
count: object
	"I prepare an object as a select in the format ""select count(1) from table [where key1 = value1 [and keyN = valueN]]"". If parameters is empty, the where clause is not specified."

	^ self query: object by: [ self count ]