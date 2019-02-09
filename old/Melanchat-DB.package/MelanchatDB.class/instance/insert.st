inserting
insert
	"I create an insert statement, returning the inserted id."

	| c |
	self asInsertStatement: ''.
	c := self execute.
	^ c first at: #id