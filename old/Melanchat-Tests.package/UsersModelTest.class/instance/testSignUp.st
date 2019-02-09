signup
testSignUp
	self should: [ (model copy password: nil) sign: nil up: nil ] raise: BadRequest.
	self should: [ (model copy email: nil) sign: nil up: nil ] raise: BadRequest.
	model insert.
	self should: [ model sign: nil up: nil ] raise: BadRequest.
	self assert: ((model email: model email , '.nz') sign: nil up: nil) isDictionary.
	self assert: ((db rawQuery: 'select count(1) from users where email = ''' , model email , '''') first at: #count) equals: 1