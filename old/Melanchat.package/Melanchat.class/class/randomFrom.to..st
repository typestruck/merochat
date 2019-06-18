utilities
randomFrom: min to: max
	"I am a convenience method for random numbers in  a (inclusive) range."

	^ (Random new next * (max - min + 1) + min) floor