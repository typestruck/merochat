requests
runJSONRequest: block withModel: class
	"I run the given closure with request and model class as parameters, serializing its output as json or setting status 400 on a BadRequest exception."

	^ [ :request | 
	[ self asJSON: (block cull: (self request: request asModel: class) cull: request) ]
		on: BadRequest
		do: [ :bad | self badRequestJSON: bad messageText ] ]