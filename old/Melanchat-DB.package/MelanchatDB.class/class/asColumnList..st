utilities
asColumnList: parameters
	"I return the keys of a dictionary as a comma separated list."

	^ ', ' join: (parameters keys reject: [ :key | (parameters at: key) isNil ] thenCollect: [ :key | key ])