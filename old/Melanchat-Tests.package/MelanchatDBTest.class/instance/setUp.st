initialization
setUp
	dummyUser := Users new
		name: 'test';
		email: 'test@email.com';
		password: 'hunter2';
		headline: 'test';
		gender: 'F';
		birthday: '2000-1-1';
		description: 'test'.
	db := MelanchatDB configure: 'config-test.json'.
	db rawQuery: 'select truncateTables()'