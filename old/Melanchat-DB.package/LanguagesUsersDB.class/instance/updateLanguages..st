updating
updateLanguages: collection
	db delete: (LanguagesUsers new speaker: speaker).
	db
		insertMultiple:
			(collection
				collect: [ :c | 
					LanguagesUsers new
						language: c;
						speaker: speaker ])