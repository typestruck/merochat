utilities
parametrize
	"I am a sql string which can be used for a prepared statement."

	| parametrizedSQL skipped |
	(parameters isNil or: [ parameters isEmpty ])
		ifTrue: [ ^ sql ].
	parametrizedSQL := sql.
	skipped := 0.
	parameters keys
		doWithIndex: [ :key :index | 
			| regex |
			regex := ('@' , key , '\b') asRegexIgnoringCase.
			parametrizedSQL := regex
				copy: parametrizedSQL
				replacingMatchesWith:
					((self class isDBNull: (parameters at: key))
						ifTrue: [ 'null' ]
						ifFalse: [ (parameters at: key) ifNil: [ skipped := skipped + 1 ] ifNotNil: [ '$' , (index - skipped) printString ] ]) ].
	^ parametrizedSQL