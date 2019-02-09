utilities
asValueList: parameters withSuffix: suffix
	" I return the keys of dictionary as @key<suffix>(in the case they are DBNull, null) comma separated list."

	^ ', '
		join:
			(parameters keys
				reject: [ :key | (parameters at: key) isNil ]
				thenCollect: [ :key | 
					(self isDBNull: (parameters at: key))
						ifTrue: [ 'null' ]
						ifFalse: [ '@' , key , suffix ] ])