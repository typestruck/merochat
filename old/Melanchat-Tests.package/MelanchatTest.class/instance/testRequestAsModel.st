utilities
testRequestAsModel
	self assert: (Melanchat request: self class emptyGET asModel: UsersModel) id equals: nil.
	"json token for UsersModel should be automatically decoded into id"
	self
		assert:
			(melanchat class
				request: (self class POST: Dictionary new withToken: (Melanchat createToken: 23))
				asModel: UsersModel) id
		equals: 23.
		
			"json token for IMModel should be automatically decoded into id"
	self
		assert:
			(melanchat class
				request: (self class POST: Dictionary new withToken: (Melanchat createToken: 23))
				asModel: IMModel) id
		equals: 23.
	self
		assert:
			(Melanchat
				request: (self class GET: (Dictionary with: #name -> 'name'))
				asModel: UsersModel) name
		equals: 'name'