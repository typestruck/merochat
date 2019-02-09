signup
sign: captchaResponse up: remoteIP
	"I register a new user, getting random values from bender. "

	| user |
	email isEmptyOrNil | password isEmptyOrNil
		ifTrue: [ BadRequest signal: 'Invalid email or password' ].
	user := (UsersDB new email: email) selectByEmail.
	user ifNotNil: [ BadRequest signal: 'Email already registered' ].
	MelanchatConfiguration development
		ifFalse: [ ((NeoJSONReader
				fromString:
					(ZnClient new
						url: 'https://www.google.com/recaptcha/api/siteverify';
						formAt: #secret put: MelanchatConfiguration captchaSecret;
						formAt: #response put: captchaResponse;
						formAt: #remoteip put: remoteIP;
						post)) at: #success)
				ifFalse: [ BadRequest signal: 'Incorrect captcha' ] ].
	^ Melanchat
		createToken:
			((UsersDB new 
						name: self generateName;
						password: (self class hashPassword: password);
						email: email;
						headline: self generateHeadline;
						description: self generateDescription;
						insert))