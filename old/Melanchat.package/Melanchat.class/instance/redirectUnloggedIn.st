utilities
redirectUnloggedIn
"I redirect unlogged in users to /login, thus protecting restricted pages."
	^ [ :request | 
	(self class idFromRequest: request)
		ifNil: [ request method = #GET
				ifTrue: [ request abort: (TeaResponse redirect location: '/login?next=' , request url asString) ]
				ifFalse: [ TeaResponse unauthorized ] ] ]