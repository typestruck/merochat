utilities
template: name with: object
"I am a hack until the frontend design is normalized and padronized."
	^ ({'landing' . 'im' . 'settings'} includes: name)
		ifTrue: [ (MustacheTemplate on: (self templateContents: name)) value: object ]
		ifFalse: [ (MustacheTemplate on: (self templateContents: 'default'))
				value: object
				partials: {(#content -> (self templateContents: name))} asDictionary ]