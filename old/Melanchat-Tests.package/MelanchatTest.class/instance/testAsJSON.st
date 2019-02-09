utilities
testAsJSON
	| now |
	"primitive values"
	self assert: (Melanchat asJSON: 23) equals: '23'.
	self assert: (Melanchat asJSON: 'hello') equals: '"hello"'.
	self assert: (Melanchat asJSON: (Dictionary with: #key -> 34.3)) equals: '{"key":34.3}'.
	"custom mappings for dates"
	now := DateAndTime now.
	self assert: (Melanchat asJSON: now) equals: '"' , now printString , '"'.
	"instvar mappings for Model classes"
	self assert: (Melanchat asJSON: UsersModel new) equals: '{}'.
	self
		assert:
			(Melanchat
				asJSON:
					(UsersModel new
						id: 23;
						name: 'name'))
		equals: '{"id":23,"name":"name"}'.
	"instvar mappings for Model classes in collections"
	self
		assert:
			(Melanchat
				asJSON:
				{	(UsersModel new
						id: 23;
						name: 'name')})
		equals: '[{"id":23,"name":"name"}]'