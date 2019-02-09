as yet unclassified
extensionFromBytes: bytes
	| type extension |
	type := #image.
	((bytes copyFrom: 1 to: 12) = #(255 216 255 224 0 16 74 70 73 70 0 1)
		or: [ (bytes copyFrom: 1 to: 4) = #(255 216 255 238) or: [ (bytes copyFrom: 1 to: 4) = #(255 216 255 225) and: [ (bytes copyFrom: 7 to: 13) = #(69 120 105 102 0 0) ] ] ])
		ifTrue: [ extension := '.jpg' ].
	(bytes copyFrom: 1 to: 8) = #(137 80 78 71 13 10 26 10)
		ifTrue: [ extension := '.png' ].
	((bytes copyFrom: 1 to: 6) = #(71 73 70 56 55 97) or: [ (bytes copyFrom: 1 to: 6) = #(71 73 70 56 57 97) ])
		ifTrue: [ extension := '.gif' ].
	(bytes copyFrom: 1 to: 2) = #(66 77)
		ifTrue: [ extension := '.bmp' ].
	((bytes copyFrom: 1 to: 4) = #(73 73 42 0) or: [ (bytes copyFrom: 1 to: 4) = #(77 77 0 42) ])
		ifTrue: [ extension := '.tiff' ].
	type := #video.
	(bytes copyFrom: 5 to: 8) = #(102 116 121 112)
		ifTrue: [ extension := '.mp4' ].
	(bytes copyFrom: 1 to: 4) = #(26 69 223 163)
		ifTrue: [ extension := '.mkv' ].
	((bytes copyFrom: 1 to: 4) = #(82 73 70 70) and: [ (bytes copyFrom: 9 to: 12) = #(65 86 73 32) ])
		ifTrue: [ extension := '.avi' ].
	(bytes copyFrom: 5 to: 12) = #(102 116 121 112 113 116 32 32)
		ifTrue: [ extension := '.mov' ].
	(bytes copyFrom: 1 to: 16) = #(48 38 178 117 142 102 207 17 166 217 0 170 0 98 206 108)
		ifTrue: [ extension := '.wmv' ].
	extension ifNotNil: [ ^ {type. extension.} ].
	^ nil