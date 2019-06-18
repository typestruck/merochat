utilities
resultSet: results
	"I return the result of a query as a collection of dictionaries or, in case of a single unnamed column, an collection of scalar values."

	| columns |

	(results class ~= PGResult or: [ results isEmpty ])
		ifTrue: [ ^ OrderedCollection new ].
	columns := results rowDescription columnDescriptions collect: [ :e | e fieldName ].
	columns size = 1 & (columns first = '?column?')
		ifTrue: [ ^ results rows collect: [ :row | row data at: 1 ] ].
	^ results rows
		collect: [ :row | 
			| dictionary |
			dictionary := Dictionary new.
			columns do: [ :col | dictionary at: col put: (row atName: col) ].
			dictionary ]