settings
updateProfile
	"I update an user's profile, i.e., their information which is publicaly visible. Empty fields get new random values from bender."

	| randomized |
	randomized := self class new.
	name
		ifNil: [ name := self generateName.
			randomized name: name ].
	headline
		ifNil: [ headline := self generateHeadline.
			randomized headline: headline ].
	description
		ifNil: [ description := self generateDescription.
			randomized description: description ].
	birthday
		ifNotNil: [ birthday := [ Date readFrom: birthday pattern: 'yyyy-m-d' ]
				on: DateError
				do: [ MelanchatDB DBNull  ] ].
	country := [ country asNumber ]
		on: Error
		do: [ MelanchatDB DBNull  ].
	UsersDB new
		id: id;
		birthday: birthday;
		name: name;
		headline: headline;
		gender: gender;
		description: description;
		country: country;
		updateLanguages: languages tags: tags.
	^ randomized