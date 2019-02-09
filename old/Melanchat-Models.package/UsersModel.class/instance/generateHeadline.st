bender
generateHeadline
	"I ask bender for text, remove anything after a period(adding ellipsis 30% of the time) to form a headline"

	| output period maxSize |
	maxSize := 100.
	output := self class
		generate: #description
		ofSize: (Melanchat randomFrom: 32 to: maxSize).
	period := output findString: '.'.
	period > 0
		ifTrue: [ output := output copyFrom: 1 to: period - 1.
			(output size <= (maxSize - 3) and: [ (Melanchat randomFrom: 1 to: 100) <= 30 ])
				ifTrue: [ output := output , '...' ] ].
	^ output