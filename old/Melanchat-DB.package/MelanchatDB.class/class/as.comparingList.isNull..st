utilities
as: parameters comparingList: token isNull: boolean
"I return parameters as a string of key = @key or key is null separated by token."

	^ token
		join:
			(parameters keys
				collect: [ :key | 
					(self isDBNull: (parameters at: key))
						ifTrue: [ key
								,
									(boolean
										ifTrue: [ ' is null' ]
										ifFalse: [ ' = null' ]) ]
						ifFalse: [ key , ' = @' , key ] ])