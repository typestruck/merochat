utilities
testAsColumnList
	self
		assert:
			(db class
				asColumnList:
					{(#name -> 'name').
					(#id -> 23)} asDictionary
)
		equals: 'name, id'.
	"ignoring nil"
	self
		assert:
			(db class
				asColumnList:
					{(#name -> nil).
					(#id -> 23)} asDictionary
)
		equals: 'id'.
