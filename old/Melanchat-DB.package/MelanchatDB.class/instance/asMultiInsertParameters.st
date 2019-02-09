inserting
asMultiInsertParameters
	"I flatten a collection of dictionaries into a single dictionary."

	| dictionary |
	dictionary := Dictionary new.
	parameters  doWithIndex: [ :d :i | d keysDo: [ :k | dictionary at: k , i asString put: (d at: k) ] ].
	parameters := dictionary