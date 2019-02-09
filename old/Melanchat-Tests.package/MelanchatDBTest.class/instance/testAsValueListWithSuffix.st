utilities
testAsValueListWithSuffix
	self
		assert:
			(db
				parameters:
					{(#id -> 23).
					(#name -> 'name')} asDictionary;
				asValueListWithSuffix: '')
		equals: '@name, @id'.
	self
		assert:
			(db
				parameters:
					{(#id -> 23).
					(#name -> 'name')} asDictionary;
				asValueListWithSuffix: '1')
		equals: '@name1, @id1'.
	"ignorinig nil"
	self
		assert:
			(db parameters: 					{(#id -> 23).
					(#name -> nil)} asDictionary;
				asValueList)

		equals: '@id'.
	"MelanchatDB DBNull as null"
	self
		assert:
			(db parameters:					{(#id -> 23).
					(#name -> db class DBNull)} asDictionary;

				asValueList)
		equals: 'null, @id'