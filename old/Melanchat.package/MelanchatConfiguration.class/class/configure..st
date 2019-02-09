instance creation
configure: newConfigFile
	(singleton isNil or: [ configFile ~= newConfigFile ])
		ifFalse: [ ^ self ].
	configFile := newConfigFile.
	singleton := self new
		configuration: (NeoJSONReader fromString: newConfigFile asFileReference contents)