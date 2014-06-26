(* Mathematica Test File *)
Test[
	readInput["test.fasta", 4, True]
	,
	{{1, 1, 4, 1}, 
	 {3, 1, 1, 1, 1}, 
	 {4, 1, 4, 1, 4}, 
	 (*These are the reverse complements, in reverse order.*)
	 {1, 4, 1, 4, 1}, 
	 {4, 4, 4, 4, 2}, 
	 {4, 1, 4, 4}}
	 ,
	TestID->"motifEMTest-20130917-M0H5A5"
]

(*Test the filtering of sequences shorter than the desired motif length.*)
Test[
	readInput["test.fasta", 5, True]
	,
	{{3, 1, 1, 1, 1},
	{4, 1, 4, 1, 4},
	 (*These are the reverse complements, in reverse order.*)
	{1, 4, 1, 4, 1}, 
	{4, 4, 4, 4, 2}}
	 ,
	TestID->"motifEMTest-20130917-B7N8L8"
]

(* Test the option of not including the reverse strand*)
Test[
	readInput["test.fasta", 5, False]
	,
	{{3, 1, 1, 1, 1},
	{4, 1, 4, 1, 4}}
	 ,
	TestID->"Tools-20130922-J7K7I3"
]

Test[
	reverseComplement[{1, 2, 3, 4, 2, 3}]
	,
	{2, 3, 1, 2, 3, 4}
	,
	TestID->"Tools-20130922-W7S5E2"
]

Test[
	maxPosition[{1.3, 2, 5, 2, -30}]
	,
	3
	,
	TestID->"Tools-20130922-N8Q7L5"
]