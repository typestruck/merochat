mapping
mappeableClasses
	^( (self classFromPackage:  'Melanchat-Models') , (self classFromPackage: 'Melanchat-DB'))
		collect: [ :e | Smalltalk classNamed: e ]