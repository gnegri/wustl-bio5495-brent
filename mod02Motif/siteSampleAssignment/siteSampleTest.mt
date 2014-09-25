(* Mathematica Test File *)

(* Testing the function siteSample, which produces a sample for the bag of sites
   problem with two types of sites. *)

(* Testing that the dimensions of the sample are correct. *)
Test[
	Dimensions[siteSample[.5, .5, {{1/4, 1/4, 1/4, 1/4}, {1/4, 1/4, 1/4, 1/4}, {1/4, 1/4, 1/4, 1/4}}, {1/5, 1/5, 1/5, 2/5}, 7]]
	,
	{7,3}
	,
	TestID->"ProbabilityTest-20130726-D6G2S8"
]

(* Testing that the sampled values lie in the correct range and that 0 probabilities are handled correctly. *)
Test[Module[{siteProbs = Map[{1/3, 1/3, 1/3, 0}&, Range[20]]},
			 sample=siteSample[1, 0, siteProbs, {1/5, 1/5, 1/5, 2/5}, 20];
	         (Max[sample] == 3 && Min[sample] == 1)]
	,
	True
	,
	TestID->"ProbabilityTest-20130726-P4O9Q3"
]

Test[Module[{siteProbs = Map[{1/3, 1/3, 1/3, 0}&, Range[20]]},
			 sample=siteSample[0, 1, siteProbs, {0, 1/5, 2/5, 2/5}, 20];
	         Max[sample]==4 && Min[sample]==2]
	,
	True
	,
	TestID->"ProbabilityTest-20130806-R4U2C8"
]

(* Testing that the relative probabilities are in the right ballpark in large samples. *)
Test[Module[{siteProbs = Map[{1/3, 1/3, 1/3, 0}&, Range[20]]},
			sample=siteSample[0, 1, siteProbs, {0, 1/5, 2/5, 2/5}, 50];
	         3*Count[sample, 2, 2] > Count[sample, 3, 2] > 1.2 * Count[sample, 2, 2]]
	,
	True
	,
	TestID->"ProbabilityTest-20130806-W3T9Q7"
]


(*Testing that two calls to siteSample produce different values.*)
Test[Module[{siteDist = Map[{1/4, 1/4, 1/4, 1/4}&, Range[8]]},
			 sample1=siteSample[1, 1, siteDist, {1/4, 1/4, 1/4, 1/4}, 50];
			 sample2=siteSample[1, 1, siteDist, {1/4, 1/4, 1/4, 1/4}, 50];
			 identicalDraws=False;
	         Do[If[sample1[[i]] == sample2[[i]],
	         	   identicalDraws = True],
	         	{i, 1, 20}];
	         identicalDraws
	         ]
	,
	False
	,
	TestID->"ProbabilityTest-20130806-K6G3Y1"
]