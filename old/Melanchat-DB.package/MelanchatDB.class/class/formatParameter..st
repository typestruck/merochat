utilities
formatParameter: parameter
	"I escape quotes and transform arrays into the syntax used by postgresql."

	parameter isString
		ifTrue: [ ^ parameter copyWithRegex: '''' matchesReplacedWith: '''''' ].
	(parameter isCollection and: [ parameter isSequenceable ])
		ifTrue: [ ^ '{' , (',' join: parameter) , '}' ].
	^ parameter