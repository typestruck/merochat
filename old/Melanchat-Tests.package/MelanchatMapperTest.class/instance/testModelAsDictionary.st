utilities
testModelAsDictionary
	| dictionary model |
	model := UsersModel  new.
	model copyFrom: dummyUser.
	
	dictionary := MelanchatMapper  modelAsDictionary: model.
	"only include instance variables which are part of the table"
	self assert: (dictionary includesKey: #db) not.
	"ignoniring nil"
	self assert: (dictionary includesKey: #avatar) not.
	"special value for db null"
	model avatar: MelanchatDB DBNull.
	self assert: ((MelanchatMapper modelAsDictionary: model) includesKey: #name).
	self assert: (dictionary at: #name) equals: 'test'