requests
runJSONRequest: block
	"I run the given closure, serializing its output as json or setting status 400 on a BadRequest exception."
	^ [ :request | 
	[  self asJSON: (block cull: request) ]
		on: BadRequest
		do: [ :bad |  self badRequestJSON: bad messageText ] ]