utilities
asValueListWithSuffix: suffix
	" I return the keys of dictionary as @key<suffix>(in the case they are DBNull, null) comma separated list."

	^ self class asValueList: parameters withSuffix: suffix