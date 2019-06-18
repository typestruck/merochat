accessing
model
	^ model ifNil: [ self class superclass new ]