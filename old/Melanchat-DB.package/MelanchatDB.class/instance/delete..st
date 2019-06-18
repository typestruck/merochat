deleting
delete: object
	"I prepare an object as a delete statement, in the format ""delete from table where key1 = value1 [and keyn = valuen ]"". If id is present in the dictionary it is used in the where clause; otherwise all keys in the dictionary are used."

	self query: object by: [ self delete ]