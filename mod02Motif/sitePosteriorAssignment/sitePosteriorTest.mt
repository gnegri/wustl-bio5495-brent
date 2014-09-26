(* Mathematica Test File *)

(* Testing the function sitePosterior, which calculates the posterior probability
   of each die type in the bag-of-sequences problem. *)

Test[
	Module[{siteFreqs = {{.25, .25, .25, .25}, {.25, .25, .25, .25}, {.25, .25, .25, .25}, {.25, .25, .25, .25}}},
		sitePosterior[{1, 1, 1, 1}, 0.5, 0.5, siteFreqs, {1/4, 1/4, 1/4, 1/4}]
		]
	,
	0.5
	,
	TestID->"sitePosteriorTest-20130809-G3P4M4"
]

(* Testing that, when the bases provide no evidence favoring either sequence type, 
the posterior is equal to the prior.*)
Test[
	Module[{siteFreqs = {{.25, .25, .25, .25}, {.25, .25, .25, .25}, {.25, .25, .25, .25}, {.25, .25, .25, .25}}},
		Round[N[sitePosterior[{1, 1, 1, 1}, 1/3, 2/3, siteFreqs, {1/4, 1/4, 1/4, 1/4}],2], .01]
		]
	,
	0.33
	,
	TestID->"sitePosteriorTest-20130809-D5P3P2"
]

(* Testing that the likelihood can overcome the prior*)
Test[
	Module[{siteFreqs = {{.1, .4, .4, .1}, {.1, .4, .4, .1}, {.8, .1, .1, 0}, {.8, .1, .1, 0}}},
		Round[sitePosterior[{2, 3, 1, 1}, 1/4, 3/4, siteFreqs, {1/4, 1/4, 1/4, 1/4}], .01]
		]
	,
	0.90
	,
	TestID->"sitePosteriorTest-20130809-J8Q2Z1"
]

(* Testing the handling of a zero in backgroundProbs.*)
Test[
	Module[{siteFreqs = {{.25, .25, .25, .25}, {.25, .25, .25, .25}, {.25, .25, .25, .25}, {1/8, 1/8, 1/8, 5/8}}}, 
 		Round[sitePosterior[{1, 3, 1, 4}, 1/3, 2/3, siteFreqs, {1/8, 2/8, 0/8, 5/8}], .01]]
	,
	1.
	,
	TestID->"sitePosteriorTest-20130809-F0L4S1"
]

(* Testing the handling of a zero in the siteProbs.*)
Test[
	Module[{siteFreqs = {{.25, .25, .25, .25}, {.25, .5, 0, .25}, {.25, 0, .5, .25}, {1/8, 1/8, 1/8, 5/8}}}, 
 		Round[sitePosterior[{1, 3, 1, 4}, 1/3, 2/3, siteFreqs, {1/4, 1/4, 1/4, 1/4}], .01]]
	,
	0.
	,
	TestID->"sitePosteriorTest-20130809-Z6Y4I2"
]

(* Testing the handling of a zero in siteProbs for an unobserved base.*)
Test[
	Module[{siteFreqs = {{.25, .25, .25, .25}, {.25, .5, 0, .25}, {.25, 0, .5, .25}, {1/8, 1/8, 1/8, 5/8}}}, 
 		Round[sitePosterior[{1, 1, 1, 4}, 1/3, 2/3, siteFreqs, {1/4, 1/4, 1/4, 1/4}], .01]]
	,
	0.56
	,
	TestID->"sitePosteriorTest-20130809-P5T2U2"
]

(* Testing the handling of seeing bases not expected for either sites or background *)
Test[
	Module[{siteFreqs = {{.25, .25, .25, .25}, {.25, .5, 0, .25}, {.25, 0, .5, .25}, {1/3, 1/3, 1/3, 0/8}}}, 
 		Round[sitePosterior[{1, 1, 1, 4}, 1/3, 2/3, siteFreqs, {1/3, 1/3, 1/3, 0}], .01]]
	,
	0.5
	,
	TestID->"zeroProbEvent"
]


(* Testing the handling of a zero in the priors, and siteProbs*)
Test[Module[{siteProbs={{.8, .1, .1, 0}, {0, .8, .1, .1}, {.1, 0, .8, .1}, {1/3, 1/3, 1/3, 0}}},
			Round[sitePosterior[{1, 2, 3, 4}, 0, 1, siteProbs, {1/4, 1/4, 1/4, 1/4}], .01]
		]
	,
	0.
	,
	TestID->"sitePosteriorTest-20130809-J8K5X1"
]