updating
update: object
	"I prepare an object as an update statement in the format ""update table set key1 = value1[, keyN = valueN] where id = @id""."

	^ self query: object by: [ self update ]