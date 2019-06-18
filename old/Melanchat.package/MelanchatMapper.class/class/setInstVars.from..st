mapping
setInstVars: model from: dictionary
	"I set the instance variables of a model object present in the dictionary."

	| object |
	object := model isClass
		ifTrue: [ model new ]
		ifFalse: [ model ].
	(object class allInstVarNames select: [ :var | dictionary includesKey: var asLowercase   ])
		do: [ :var | object instVarNamed: var put: (dictionary at: var asLowercase ) ].
	^ object