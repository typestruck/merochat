inserting
insert
"I am a convenience for inserting."
	id := self db insert: self asTableClass.
	^ id