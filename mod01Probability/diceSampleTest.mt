(* Mathematica Test File *)

(* Testing the function diceSample, which produces a sample for the bag of dice
   problem with two types of dice. *)

(* Testing that the dimensions of the sample are correct. *)
Test[
	Dimensions[diceSample[1, 1, {1/4, 1/4, 1/4, 1/4}, {1/5, 1/5, 1/5, 2/5}, 7, 10]]
	,
	{7,10}
	,
	TestID->"ProbabilityTest-20130726-D6G2S8"
]

(* Testing that the sampled values lie in the correct range and that 0 probabilities are handled correctly. *)
Test[Module[{sample=diceSample[1, 0, {1/3, 1/3, 1/3, 0}, {1/5, 1/5, 1/5, 2/5}, 20, 20]},
	         (Max[sample] == 3 && Min[sample] == 1)]
	,
	True
	,
	TestID->"ProbabilityTest-20130726-P4O9Q3"
]

Test[Module[{sample=diceSample[0, 1, {1/3, 1/3, 1/3, 0}, {0, 1/5, 2/5, 2/5}, 20, 20]},
	         Max[sample]==4 && Min[sample]==2]
	,
	True
	,
	TestID->"ProbabilityTest-20130806-R4U2C8"
]

(* Testing that the relative probabilities are in the right ballpark in large samples. *)
Test[Module[{sample=diceSample[0, 1, {1/3, 1/3, 1/3, 0}, {0, 1/5, 2/5, 2/5}, 50, 20]},
	         3*Count[sample, 2, 2] > Count[sample, 3, 2] > 1.2 * Count[sample, 2, 2]]
	,
	True
	,
	TestID->"ProbabilityTest-20130806-W3T9Q7"
]


(*Testing that two calls to diceSample produce different values.*)
Test[Module[{sample1=diceSample[1, 1, {1/4, 1/4, 1/4, 1/4}, {1/4, 1/4, 1/4, 1/4}, 50, 20],
			 sample2=diceSample[1, 1, {1/4, 1/4, 1/4, 1/4}, {1/4, 1/4, 1/4, 1/4}, 50, 20],
			 identicalDraws=False},
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