im
start: chattingID
	"I set up the initial state of IM."

	user := UsersModel new
		id: self id;
		formatForLoggedin.
	suggestions := self suggest.
	self chattingWith: chattingID.
	contacts := HistoriesDB new
		sender: id;
		selectContacts