requests
badRequestJSON: message
	"I reply a 400 status code with the json payload of {  errorMessage : string  }."

	^ TeaResponse badRequest body: (self asJSON: (Dictionary with: #errorMessage -> message))