converting
asJSON: object
	"I convert an object to JSON, including custom mappings for dates, Model and DB classes."

	^ String
		streamContents: [ :stream | 
			| json |
			json := NeoJSONWriter on: stream.
			json
				for: DateAndTime customDo: [ :mapping | mapping encoder: #printString ];
				for: Date customDo: [ :mapping | mapping encoder: #printString ].
			MelanchatMapper  mappeableClasses do: [ :class | json mapAllInstVarsFor: class ].
			json nextPut: object ]