db
execute
	"I execute an sql query which can be parametrized by using @variable."

self cleanParameters.
	^ self class
		resultSet:
			(self
				withConnection: [ :connection | 
					| statement |

					statement := connection prepare: self parametrize.
					self bind: statement.
					statement execute ])