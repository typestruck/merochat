messages
newMessage
	"I insert a new message and return a dictionary to be transmitted via the sockets."

	media
		ifNotNil: [ | typeExtension |
			typeExtension := self class extensionFromBytes: media.
			typeExtension
				ifNotNil: [ | fileName |
					fileName := UUID new printString , typeExtension second.
					(MelanchatConfiguration publicRootFolder , '/media/' , fileName) asFileReference
						writeStreamDo: [ :stream | 
							stream
								binary;
								nextPutAll: media asByteArray ].
					content := typeExtension first = #image
						ifTrue: [ "public/media needs to be in config..." '![' , fileName , '](' , fileName , ')' ]
						ifFalse: [ '<video controls src="/public/media/' , fileName , '"></video>' ] ] ].
	id := MessagesDB new
		sender: self sender;
		recipient: self recipient;
		content: content;
		insert.
	^ Dictionary
		with:
			#sender
				->
					(self class new
						id: id;
						sender: recipient;
						temporaryID: temporaryID;
						type: 2)
		with:
			#recipient
				->
					(self class new
						id: id;
						sender: sender;
						content: content;
						type: 4)