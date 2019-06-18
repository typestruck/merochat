user
avatarPath: id
	| custom |
	custom := id asString , '/avatar.png'.
	(MelanchatConfiguration publicRootFolder asFileReference / 'media' / custom) exists
		ifTrue: [ ^ '/public/media/' , custom ]
		ifFalse: [ ^ '/public/media/avatar.png' ]