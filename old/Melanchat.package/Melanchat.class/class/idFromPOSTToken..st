requests
idFromPOSTToken: token
	"I extract an user id from a json web token sent in a POST request."

	| materialized |
	token ifEmpty: [ ^ nil ].
	materialized := [ JsonWebSignature materializeCompact: token key: MelanchatConfiguration tokenSecret2 ]
		on: Error
		do: [ :e | ^ nil ].
	^ (materialized payload at: #user) asNumber