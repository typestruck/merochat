instance creation
configure: file
"I start the Melanchat server using the given config file. "
	^ (self new configFile: file) start