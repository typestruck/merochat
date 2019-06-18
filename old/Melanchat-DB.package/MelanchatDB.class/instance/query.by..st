db
query: object by: block
	"I prepare object for being executed as a sql query, calling either block or self execute."
sql := nil.
parameters  := nil.
	object class = QueryParameters
		ifTrue: [ sql := object query.
			parameters := object parameters.
			^ self execute ].
	object isString
		ifTrue: [ sql := object.
			parameters := Dictionary new ]
		ifFalse: [ object isCollection
				ifTrue: [ table := object first class name.
					parameters := object collect: [ :c | MelanchatMapper  modelAsDictionary: c ] ]
				ifFalse: [ table := object class name.
					parameters := MelanchatMapper  modelAsDictionary: object ] ].
	^ block value