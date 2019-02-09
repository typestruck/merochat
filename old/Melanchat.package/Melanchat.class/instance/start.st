initialization
start
	imSockets := Dictionary new.
	MelanchatConfiguration configure: configFile.
	(Teapot
		stopAll;
		configure:
			{(#debugMode -> MelanchatConfiguration development).
			(#port -> MelanchatConfiguration port)})
		before: '/' -> self redirectLoggedIn;
		before: '/login' -> self redirectLoggedIn;
		before: '/register' -> self redirectLoggedIn;
		before: '/recover/*' -> self redirectLoggedIn;
		GET: '/' -> self landingPage;
		POST: '/register' -> self signup;
		GET: '/login' -> self loginPage;
		POST: '/login' -> self login;
		GET: '/recover' -> self recoverPage;
		POST: '/recover/reset' -> self reset;
		POST: '/recover' -> self recover;
		before: '/im/*' -> self redirectUnloggedIn;
		POST: '/im/archive' -> self archiveChat;
		POST: '/im/read' -> self readMessages;
		POST: '/im/unread' -> self unreadMessages;
		GET: '/im/chat/history' -> self chatHistory;
		GET: '/im/chat/<chatting>' -> self imPage;
		GET: '/im/socket' -> self fromSocket;
		GET: '/im' -> self imPage;
		before: '/user/*' -> self redirectUnloggedIn;
		GET: '/user/display/<id>' -> self display;
		POST: '/user/update/profile' -> self updateProfile;
		POST: '/user/update/settings' -> self updateSettings;
		POST: '/user/update/settings/enter' -> self updateMessageOnEnter;
		POST: '/user/update/settings/recentEmoji' -> self updateRecentEmoji;
		before: '/settings/*' -> self redirectUnloggedIn;
		GET: '/settings' -> [ :request | self settingsPage: request ];
		GET: '/settings/profile' -> [ :request | self settingsPage: request ];
		serveStatic: '/public' from: MelanchatConfiguration publicRootFolder;
		start