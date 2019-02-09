upserting
upsertTags: collection


	db upsertMultiple: (collection collect: [ :c | Tags new name: c ]) conflict: 'on conflict(name) do nothing '.


	

	^ (db
		rawQuery:
			(QueryParameters
				query: 'select id from tags where name = any(@names) order by name'
				parameters: (Dictionary with: #names -> collection))) collect: [ :dictionary | dictionary at: #id ]