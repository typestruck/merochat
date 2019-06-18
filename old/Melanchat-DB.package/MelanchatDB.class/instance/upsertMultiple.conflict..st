upserting
upsertMultiple: object conflict: statement
	"I prepare an object as a multivalued insert statement with a conflict clause in the format ""insert into table(keys) values(values) conflict returning id""."

	^ self query: object by: [ self upsertMultiple: statement ]