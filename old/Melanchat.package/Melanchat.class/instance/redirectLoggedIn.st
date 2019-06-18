utilities
redirectLoggedIn
	"I redirect logged in users back to /im."

	^ [ :request | 
	(self class idFromRequest: request) ifNotNil:  [ request method = #GET
				ifTrue: [ request abort: (TeaResponse redirect location: '/im') ]
				ifFalse: [ self class badRequestJSON: 'Already authorized' ] ] ]