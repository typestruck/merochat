utilities
asValueList
	" I return the keys of dictionary as @key(in the case they are DBNull, null) comma separated list."

	^ self asValueListWithSuffix: ''