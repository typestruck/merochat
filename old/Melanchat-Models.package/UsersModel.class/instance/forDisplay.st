user
forDisplay
	"I format the fields to be displayed as public information."

	avatar := self class avatarPath: id.
	languages := (languages isNil or: [ languages isString not ])
		ifTrue: [ {} ]
		ifFalse: [ languages splitOn: ',' ].
	tags := (tags isNil or: [ tags isString not ])
		ifTrue: [ {} ]
		ifFalse: [ tags splitOn: '\n' ].
		
	recentEmoji isNotNil ifTrue: [ recentEmoji := recentEmoji splitOn: ' '  ]