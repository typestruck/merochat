updating
updateLanguages: userLanguages tags: userTags
	self db
		withTransaction: [ :transDB | 
			userLanguages
				ifNotEmpty: [ (LanguagesUsersDB db: transDB)
						speaker: id;
						updateLanguages: (userLanguages splitOn: ',') ].
			userTags
				ifNotEmpty: [ (TagsUsersDB db: transDB)
						creator: id;
						updateTags: (userTags splitOn: String lf) ].
			self update ]