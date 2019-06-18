utilities
bind: statement
	"I set the parameters of a prepared statement, ignoring DBNull entries."

	(parameters keys reject: [ :key | self class isDBNull: (parameters at: key) ])
		doWithIndex: [ :key :index | statement at: index bind: (self class formatParameter: (parameters at: key)) ]