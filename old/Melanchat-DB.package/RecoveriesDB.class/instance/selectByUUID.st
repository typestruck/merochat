selecting
selectByUUID
	^ self db select: (Recoveries new uuid: uuid) into: self model