db
rawQuery: object
	"I run string or QueryParameters sql queries with no further processing."

	^ self query: object by: [ self execute ]