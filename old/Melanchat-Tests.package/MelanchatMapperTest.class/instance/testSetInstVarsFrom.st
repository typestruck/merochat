utilities
testSetInstVarsFrom
	| user dictionary |
	user := UsersModel new.
	dictionary := {(#id -> 23).
	(#name -> 'name').
	(#avatar -> 'avatar.png')} asDictionary.
	MelanchatMapper  setInstVars: user from: dictionary.
	self assert: user id equals: (dictionary at: #id).
	self assert: user avatar equals: (dictionary at: #avatar).
	self assert: user name equals: (dictionary at: #name).
	"using class"	
	self assert:(MelanchatMapper setInstVars: Users from: dictionary) id equals: (dictionary at: #id).