selecting
select: object into: projection
	"I prepare an object as a select statementin the format ""select * from table where key1 = value1 [and keyN = valueN]"". The result is injected into projection if projection is an instance object, returned as a an object if projection is a class or as a collection if projection is a collection."

	| results |
	results := self query: object by: [ self select ].
	results isEmptyOrNil
		ifTrue: [ 
			^ projection isCollection ifTrue: [ OrderedCollection new ] ifFalse: [ nil ]
			].
	^ projection isCollection
		ifTrue: [ results collect: [ :dictionary | MelanchatMapper  setInstVars: projection first new from: dictionary ] ]
		ifFalse: [ MelanchatMapper 
				setInstVars:
					(projection isClass
						ifTrue: [ projection new ]
						ifFalse: [ projection ])
				from: results first ]