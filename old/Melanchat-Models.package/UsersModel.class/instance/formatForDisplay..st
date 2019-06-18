user
formatForDisplay: object
	"I format the fields to be displayed as public information."

(	object isNumber or: [ object isNil  ])
		ifTrue: [ (UsersDB model: self)
				id: id;
				selectForDisplay ]
		ifFalse: [ self copyFrom: object ].
	self forDisplay