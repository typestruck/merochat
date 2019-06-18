selecting
asSelectStatement
sql := 
 'select * from ' , table , ' where ' , (self asComparingList: ' and ')