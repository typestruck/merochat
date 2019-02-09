recover
recover
	"I send an email with a link to reset an user's password."

	| uuid smtpClient mailMessage recoverURL |
	(UsersDB new email: email) selectByEmail
		ifNil: [ BadRequest signal: 'Email not registered' ]
		ifNotNil: [ :user | self copyFrom: user ].
	uuid := UUID new printString.
	recoverURL := MelanchatConfiguration url , '/recover?u=' , uuid.
	RecoveriesDB new
		recoverer: id;
		uuid: uuid;
		insert.
	mailMessage := MailMessage empty.
	mailMessage
		setField: 'to' toString: email;
		setField: 'from' toString: 'recover@melan.chat';
		setField: 'subject' toString: 'Melanchat password recovery';
		setField: 'content-type' toString: 'text/html; charset=utf8';
		setField: 'mime-version' toString: '1.0';
		body:
			(MIMEDocument
				contentType: 'text/html'
				content:
					'Hello there!<br> <a href="' , recoverURL
						, '">You can set up a new password here</a><br>Copy and paste the following url if the link is not working: '
						, recoverURL).
	smtpClient := ZdcSecureSMTPClient new.
	MelanchatConfiguration development ifFalse: [
	smtpClient
		user: MelanchatConfiguration email;
		password: MelanchatConfiguration emailPassword;
		openOnHost: (NetNameResolver addressForName: MelanchatConfiguration emailServer) port: 465;
		mailFrom: MelanchatConfiguration email to: {email} text: mailMessage text;
		quit;
		close].
	^ Melanchat ok