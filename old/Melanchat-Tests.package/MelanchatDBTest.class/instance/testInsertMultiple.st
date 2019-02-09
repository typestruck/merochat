inserting
testInsertMultiple
	self
		assert:
			((db
				insertMultiple:
					{dummyUser asTableClass.
					dummyUser asTableClass.
					dummyUser asTableClass}) allSatisfy: [ :c | c isNumber ]).
					
			self
		assert:
(			(db
				rawQuery: 'select count(1) from users' ) first at: #count	) equals: 3
			