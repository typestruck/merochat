selecting
select
sql ifNil: [ 
	self asSelectStatement].
	^ self execute