copying
asTableClass
"I return a model class as its superclass or myself."
	self class superclass ~= Table
		ifTrue: [ | tableClass |
			tableClass := self class superclass new.
			tableClass class allInstVarNames do: [ :var | tableClass instVarNamed: var put: (self instVarNamed: var) ].
			^ tableClass ].
	^ self