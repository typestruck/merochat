utilities
isNightTime
	"I consider to be nighttime from 17 to 6, when the night theme css should be delivered."

	| hour |
	hour := DateAndTime now hour24.
	^ hour < 6 or: [ hour > 17 ]