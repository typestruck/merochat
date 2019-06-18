requests
runRequest: block withModel: class
	"I run the given closure with request and model class as parameters."

	^ [ :request | block cull: (self request: request asModel: class) cull: request ]