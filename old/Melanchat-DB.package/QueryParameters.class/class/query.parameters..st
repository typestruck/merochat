initialization
query: sql parameters: object
	^ self new
		query: sql;
		parameters:
			(object isDictionary
				ifFalse: [  MelanchatMapper
					 modelAsDictionary: object  ]
				ifTrue: [object ])