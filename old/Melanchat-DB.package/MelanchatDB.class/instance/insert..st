inserting
insert: object
"I prepare an object as a insert statement, in the format ""insert into from table(keys) values(values) returning id""."


	^ self query: object by: [ self insert ]