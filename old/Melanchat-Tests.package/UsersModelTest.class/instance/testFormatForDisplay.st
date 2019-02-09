user
testFormatForDisplay
	| id |
	id := dummyUser insert.
	model formatForDisplay: id.
	"formated fields"
	model
		formatForDisplay:
			(model
				languages: '23,34,55';
				tags: '1\n2').
	self assert: model languages equals: #('23' '34' '55') asOrderedCollection.
	self assert: model tags equals: #('1' '2') asOrderedCollection