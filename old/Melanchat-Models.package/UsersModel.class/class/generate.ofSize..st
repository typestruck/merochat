bender
generate: what ofSize: size
	"I call into Bender to generate text, or return <what>-test-<number> if the useBender configuration entry is set to false."

	^ MelanchatConfiguration useBender
		ifTrue: [ NeoJSONReader
				fromString:
					(ZnClient new
						get: MelanchatConfiguration benderURL , 'generate?what=' , what , '&max-chars=' , size asString) ]
		ifFalse: [ what , '-test-', (Melanchat randomFrom: 1 to: 1000000) asString  ]