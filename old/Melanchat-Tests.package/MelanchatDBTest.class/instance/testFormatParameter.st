utilities
testFormatParameter
	self assert: (db class formatParameter: 'isn''t she lovely isn''t she wonderful') equals: 'isn''''t she lovely isn''''t she wonderful'.
	self assert: (db class formatParameter: 23) equals: 23.
	self assert: (db class formatParameter: #(23 3 5 6)) equals: '{23,3,5,6}'.
	self assert: (db class formatParameter: true) equals: true