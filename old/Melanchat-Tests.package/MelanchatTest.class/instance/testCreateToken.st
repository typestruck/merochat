utilities
testCreateToken
	| token |
	token := (Melanchat createToken: 23) .
	"we should have keys #token1 and #token2 which are created with different seeds"
	self assert: ((token includesKey: #tokenGET) and: [ token includesKey: #tokenPOST ]).
	self assert: (token at: #tokenGET) ~= (token at: #tokenPOST)