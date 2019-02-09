settings
testUpdateProfile
	| user |
	dummyUser insert.
	user := UsersModel new.
	user
		id: dummyUser id;
		birthday: '1990-09-12';
		languages: #();
		tags: #();
		updateProfile.
	self assert: user birthday class equals: Date.
	"should generate"
	self assert: user name isNotNil.
	self assert: user headline isNotNil.
	self assert: user description isNotNil