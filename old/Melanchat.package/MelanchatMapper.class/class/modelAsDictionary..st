mapping
modelAsDictionary: object
	"I collect instance variables into a dictionary, ignoning nil values and Table instance variables."

	^ ((object class allInstVarNames reject: [ :var | (Table isNotMappeableInstVar: var) or: [ (object instVarNamed: var) isNil ] ])
		collect: [ :var | var -> (object instVarNamed: var) ]) asDictionary