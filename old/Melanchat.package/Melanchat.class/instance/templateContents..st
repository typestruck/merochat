utilities
templateContents: templateName
"I return the contents of a mustache template."
	^ MelanchatConfiguration templatesFolder asFileReference / (templateName , '.mustache')
		readStreamDo: [ :stream | stream contents ]