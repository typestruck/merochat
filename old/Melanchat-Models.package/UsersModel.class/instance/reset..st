recover
reset: uuid
"I reset an user's password."
	| recovery |
	password isEmptyOrNil
		ifTrue: [ BadRequest signal: 'Password required' ].
	recovery := RecoveriesDB new
		uuid: uuid;
		selectByUUID.
	recovery ifNil: [ BadRequest signal: 'Invalid recovery link' ].
	(self class isInvalidRecovery: recovery)
		ifTrue: [ BadRequest signal: 'Expired recovery link' ].
	UsersDB new
		password: (self class hashPassword: password);
		id: recovery recoverer;
		updateRecovered: recovery id.
	^ Melanchat ok