utilities
respondTemplate: name extraValues: association
	"I return a mustache template page, adding extraValues to the default values expect by all templates"

	| values |
	values := (association asDictionary)
		collect: [ :value | self class asJSON: value ].
		
	values at:	#nightTime put: self class isNightTime.
		
	^ ZnResponse ok: (ZnStringEntity with: (self template: name with: values) type: ZnMimeType textHtml)