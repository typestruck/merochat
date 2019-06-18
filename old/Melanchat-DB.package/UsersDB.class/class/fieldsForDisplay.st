selecting
fieldsForDisplay
	"I select the public fields from users. I expect the users table to have an u alias."

	^ '  u.id, gender, birthday, name, headline, description, country, (select string_agg(l.id::text, '',''  order by name) from languages l join languagesUsers lu on l.id = lu.language and lu.speaker = u.id ) languages, (select string_agg(name, ''\n'' order by name) from tags l join tagsUsers tu on l.id = tu.tag and tu.creator = u.id ) tags '