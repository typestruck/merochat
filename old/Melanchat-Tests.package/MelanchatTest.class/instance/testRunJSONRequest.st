requests
testRunJSONRequest


	self assert: ((self class requestBodyAsDictionary: ((Melanchat runJSONRequest: [ BadRequest signal: 'error!' ]) cull: nil)) at: #errorMessage) equals: 'error!'.	"BadRequest should give back a 400"
	self assert: ((Melanchat runJSONRequest: [ 23 ]) cull: nil) equals: (NeoJSONWriter toString: 23)