requests
idFromGETToken: token
	"I extract an user id from a json web token sent in a GET request."

	| materialized |
	token ifEmpty: [ ^ nil ].
	materialized := [ JsonWebSignature materializeCompact: token key: MelanchatConfiguration tokenSecret ]
		on: Error
		do: [ :e | ^ nil ].
	^ (materialized payload at: #user) asNumber