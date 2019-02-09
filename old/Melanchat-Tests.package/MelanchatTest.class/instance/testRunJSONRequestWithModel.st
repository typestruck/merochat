requests
testRunJSONRequestWithModel
self assert: ((self class requestBodyAsDictionary: ((Melanchat runJSONRequest: [ BadRequest signal: 'error!' ] withModel: UsersModel) value: self class emptyPOST)) at: #errorMessage) equals: 'error!'.	"BadRequest should give back a 400"
	self assert: ((Melanchat runJSONRequest: [ :user :request | user id ] withModel: UsersModel) value: (self class POST: Dictionary new withToken: (Melanchat createToken: 23))) equals: (NeoJSONWriter toString: 23).
	self assert: ((Melanchat runJSONRequest: [ :user  :request | user name ] withModel: UsersModel) value: (self class POST: (Dictionary with: #name -> 'name'))) equals: (NeoJSONWriter toString: 'name')