inserting
insertMultiple
	"I create a multivalued insert, returning the inserted ids."

	self asMultiInsertStatement: ''.
	^ self execute collect: [ :c | c at: #id ]