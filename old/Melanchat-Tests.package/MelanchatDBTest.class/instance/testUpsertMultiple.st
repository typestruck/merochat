upserting
testUpsertMultiple
	self
		assert:
			((db
				upsertMultiple:
					{(Tags new name: 'tag').
					(Tags new name: 'tag2').
					(Tags new name: 'tag3').
										(Tags new name: 'tag3')}
				conflict: 'on conflict(name) do nothing') allSatisfy: [ :c | c isNumber ]).
	self assert: (db count: Tags new) equals: 3