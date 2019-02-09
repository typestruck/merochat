updating
testUpdateLanguagesTags
	dummyUser insert.
	dummyUser updateLanguages: '1,2' tags: 'tag' , String lf , 'tag2'.
	self assert: (db count: LanguagesUsers new) equals: 2.
	self assert: (db count: Tags new) equals: 2.
	self assert: (db count: TagsUsers new) equals: 2