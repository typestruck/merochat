updating
updateTags: tagNames
|x|
	db delete: (TagsUsers new creator: creator).
	
x :=	((TagsDB db: db) upsertTags: tagNames) collect: [ :tagID | (TagsUsers new creator: creator) tag: tagID ].

	db
		insertMultiple: (x)