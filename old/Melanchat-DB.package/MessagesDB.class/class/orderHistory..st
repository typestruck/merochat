im
orderHistory: query
	^ 'select * from (' , query , ' order by date desc limit 20) sq order by date asc'