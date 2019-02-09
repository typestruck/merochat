user-settings
settingsPage: request
	| id |
	id := self class idFromRequest: request.
	^ self respondTemplate: 'settings' extraValues: {(#user -> (UsersModel new id: id; formatForDisplay: id))}