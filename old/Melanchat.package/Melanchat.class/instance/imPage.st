im
imPage
	^ self class
		runRequest: [ :im :request | 
			im start: (request at: #chatting ifAbsent: [ nil ]).
			self
				respondTemplate: 'im'
				extraValues:
					{(#user -> im user).
					(#contacts -> im contacts).
					(#chatting -> im chatting).
					(#suggestions -> im suggestions)} ]
		withModel: IMModel