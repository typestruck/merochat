upserting
upsertMultiple: conflict

	self asMultipleUpsertStatement: conflict.
	^ self execute
		select: [ :dictionary | dictionary includesKey: #id ]
		thenCollect: [ :dictionary | dictionary at: #id ]