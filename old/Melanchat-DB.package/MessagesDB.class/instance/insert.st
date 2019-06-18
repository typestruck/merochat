inserting
insert
	"I upsert histories and insert a new message."

	^ self db
		withTransaction: [ :transDB | 
			(HistoriesDB db: transDB)
				sender: sender;
				recipient: recipient;
				insert.
			db := transDB.
			super insert ]