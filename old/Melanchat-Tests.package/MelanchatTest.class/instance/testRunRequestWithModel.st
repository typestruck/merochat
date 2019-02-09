requests
testRunRequestWithModel
	(melanchat class
		runRequest: [ :user :request | 
			self assert: user class equals: UsersModel.
			self assert: request class equals: TeaRequest ]
		withModel: UsersModel) value: (self class POST: Dictionary new withToken: (Melanchat createToken: 23))