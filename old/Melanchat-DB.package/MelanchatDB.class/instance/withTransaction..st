db
withTransaction: block
	"I run a block as a transaction."

	transactionConnection := GADriver fromConnectionString: connectionString.
	transactionConnection connect.
	 [ [
			|returnValue|
			 transactionConnection beginTransaction.
	returnValue := block value: self.
	transactionConnection commitTransaction.
	^ returnValue ]
		on: Error
		do: [ :e | 
			transactionConnection rollbackTransaction.
			Error signal: e messageText ] ]
		ensure: [ transactionConnection close ]