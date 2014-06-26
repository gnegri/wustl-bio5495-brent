(* Mathematica Test File *)


Test[rowWindowSums[{1., 2., 3., 4., 5.}, 2]
	,
	{1., 3., 5., 7., 9.}
	,
	TestID->"motifEMTest-20130920-E6D7R8"
]

Test[rowWindowMaxes[{4.`, 7.`, 5.`, 3.`}, 2]
	,
	{7., 7., 5., 3.}
	,
	TestID->"motifEMTest-20130920-G7Z6I3"
]

Test[
	normalizePosteriorWindows[{{.3, .6, .9}, {.6, .9, .1}}, 2]
	,
	{{.3, .4, .6}, {.4, .6, .1}}
	,
	TestID->"motifEMTest-20130919-T2P4S3"
]

Test[rowUpdateErasers[{1., 1., 1., 1., 1., 1.}, {.1, .2, .3, .4, .5}, 2.]
	,
	{0.9, 0.72, 0.56, 0.42, 0.3, 0.5}
	,
	TestID->"NormalizePosteriors-20130922-H0P2W1"
]

Test[
	rowUpdateErasers[{1., 1., 1., 1., 1., 1.}, {.1, .2, 1.0, .4, .5}, 2.]
	,
	{0.9, 0.72, 0., 0., 0.3, 0.5}
	,
	TestID->"NormalizePosteriors-20130921-E4Q7F2"
]

Test[
	rowUpdateErasers[{1., 1., 0.5, 1., 1., 0.5}, {.1, .2, .3, .4, .5}, 2.]
	,
	{0.9, 0.72, 0.28, 0.42, 0.3, 0.25}
	,
	TestID->"NormalizePosteriors-20130921-P8Q4E7"
]

Test[updateErasers[{{1.0, 1.0, 0.5, 1.0, 1.0, 0.5}, {1.0, 1.0, 1.0, 1.0, 1.0}}, 
	               {{0.1, 0.2, 0.3, 0.4, 0.5},      {0.1, 0.2, 1.0, 0.4}}, 
	               2.0]
	,
	{{0.9, 0.72, 0.28, 0.42, 0.3, 0.25}, {0.9, 0.72, 0., 0., 0.6}}
	,
	TestID->"NormalizePosteriors-20130922-Q7T8X6"
]