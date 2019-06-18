inserting
insertMultiple: collection
	"I prepare an collection as a multivalued insert statement, in the format ""insert into from table(keys) values(values1), (valuesN) returning id""."

	^ self query: collection by: [ self insertMultiple ]