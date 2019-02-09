recover
testReset
	| uuid |

	model insert.
	self should: [ (model copy password: nil) reset: nil ] raise: BadRequest.
	self should: [ model reset: '' ] raise: BadRequest.
		"expired recovery"
			uuid := UUID new printString.
	Recoveries new
		uuid: uuid;
		recoverer: model id;
		created: DateAndTime now - (60 * 60 * 24 * 2);
		insert.
	self should: [(model reset: uuid)] raise: BadRequest .
	"ok"	
			uuid := UUID new printString.
	
	Recoveries new
		uuid: uuid;
		recoverer: model id;
		insert.
	self assert: (model reset: uuid) equals: Melanchat ok.
	"used recovery"
	self should: [(model reset: uuid)] raise: BadRequest 
