accessing
db
	^ db ifNil: [ MelanchatDB new ]