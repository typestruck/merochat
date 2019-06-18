db
withConnection: block
	"I run a block inside of a connection, ensuring it is closed afterwards."

	| connection |
	^ transactionConnection
		ifNil: [ connection := GADriver fromConnectionString: connectionString.
			connection connect.
			 [ block value: connection ]
				ensure: [ connection close ] ]
		ifNotNil: [ block value: transactionConnection ]