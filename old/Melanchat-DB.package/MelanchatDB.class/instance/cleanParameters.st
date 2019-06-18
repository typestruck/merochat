utilities
cleanParameters
	"I transform parameters into a dictionary if it is a collection and remove keys not present in sql from parameters."

	| keys |
	parameters isDictionary
		ifFalse: [ self asMultiInsertParameters ].
	keys := parameters keys.
	keys
		do: [ :key | 
			(('@' , key , '\b') asRegexIgnoringCase search: sql)
				ifFalse: [ parameters removeKey: key ] ]