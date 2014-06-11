(* Mathematica Test File *)

(* Testing the function diceSample, which produces a sample for the bag of dice
   problem with two types of dice.

(* Testing that the dimensions of the sample are correct. *)
Test[
	Dimensions[diceSample[1, 1, {1/4, 1/4, 1/4, 1/4}, {1/5, 1/5, 1/5, 2/5}, 7, 10]]
	,
	{7,10}
	,
	TestID->"ProbabilityTest-20130726-D6G2S8"
]

(* Testing that the sampled values lie in the correct range and that 0 probabilities are handled correctly. *)
Test[Module[{sample=diceSample[1, 0, {1/4, 1/4, 1/4, 0}, {1/5, 1/5, 1/5, 2/5}, 20, 20]},
	         (Max[sample] == 3 && Min[sample] == 1)]
	,
	True
	,
	TestID->"ProbabilityTest-20130726-P4O9Q3"
]

Test[Module[{sample=diceSample[0, 1, {1/4, 1/4, 1/4, 0}, {0, 1/5, 2/5, 2/5}, 20, 20]},
	         Max[sample]==4 && Min[sample]==2]
	,
	True
	,
	TestID->"ProbabilityTest-20130806-R4U2C8"
]

(* Testing that the relative probabilities are in the right ballpark in large samples. *)
Test[Module[{sample=diceSample[0, 1, {1/4, 1/4, 1/4, 0}, {0, 1/5, 2/5, 2/5}, 50, 20]},
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


(* Testing dicePosterior *)
(* Testing that if all counts and probabilities are equal the posterior is 1/2.*)

Test[
	N[dicePosterior[{1, 1, 1, 1}, 0.5, 0.5, {1/4, 1/4, 1/4, 1/4}, {1/4, 1/4, 1/4, 1/4}], 2]
	,
	0.50
	,
	TestID->"dicePosteriorTest-20130809-G3P4M4"
]

(* Testing that, when the counts provide no evidence favoring either die type, the posterior is equal to the prior.*)
Test[
	N[dicePosterior[{1, 1, 1, 1}, 1/3, 2/3, {1/4, 1/4, 1/4, 1/4}, {1/4, 1/4, 1/4, 1/4}], 2]
	,
	0.33
	,
	TestID->"ProbabilityTest-20130809-D5P3P2"
]

(* Testing that the likelihood can overcome the prior*)
Test[
	N[dicePosterior[{1, 1, 1, 4}, 1/3, 2/3, {1/8, 1/8, 1/8, 5/8}, {1/4, 1/4, 1/4, 1/4}], 2]
	,
	0.71
	,
	TestID->"ProbabilityTest-20130809-J8Q2Z1"
]

(* Testing the handling of a zero in the counts but not in the probabilities.*)
Test[
	N[dicePosterior[{1, 0, 1, 4}, 1/3, 2/3, {1/8, 1/8, 1/8, 5/8}, {1/4, 1/4, 1/4, 1/4}], 2]
	,
	0.83
	,
	TestID->"ProbabilityTest-20130809-R2Z7E9"
]

(* Testing the handling of a zero in the probabilities but not in the counts.*)
Test[
	dicePosterior[{1, 1, 1, 4}, 1/3, 2/3, {0, 1/8, 1/8, 6/8}, {1/4, 1/4, 1/4, 1/4}]
	,
	0
	,
	TestID->"ProbabilityTest-20130809-F0L4S1"
]

(* Testing the handling of a zero in both the probabilities and the counts.*)
Test[
	N[dicePosterior[{0, 1, 1, 4}, 1/3, 2/3, {0, 1/8, 1/8, 6/8}, {1/4, 1/4, 1/4, 1/4}],2]
	,
	0.91
	,
	TestID->"ProbabilityTest-20130809-Z6Y4I2"
]

(* Testing the handling of a zero in both the probabilities and the counts.*)
Test[
	N[dicePosterior[{0, 1, 1, 4}, 1/3, 2/3, {0, 1/8, 1/8, 6/8}, {1/4, 0, 1/4, 2/4}], 2]
	,
	1.0
	,
	TestID->"ProbabilityTest-20130809-P5T2U2"
]

(* Testing the handling of a zero in the priors, counts, and conditionals*)
Test[
	dicePosterior[{0, 1, 1, 4}, 0, 1, {0, 1/8, 1/8, 6/8}, {1/4, 1/4, 1/4, 1/4}]
	,
	0
	,
	TestID->"ProbabilityTest-20130809-J8K5X1"
]

(* Testing that dice with different numbers of faces are handled properly*)
Test[
	Round[dicePosterior[{1, 1, 1, 4, 1, 1, 1}, 1/3, 2/3, {0.1, 0.1, 0.1, 0.4, 0.1, 0.1, 0.1}, {1/7, 1/7, 1/7, 1/7, 1/7, 1/7, 1/7}], 
		0.01]
	,
	0.78
	,
	TestID->"ProbabilityTest-20130809-R5O9V9"
]
*)
(* Testing diceEM on a known input sample.*)

Test[
	Round[diceEM[{{2, 3, 3, 4, 2, 4, 3, 3, 2, 4}, 
		          {3, 1, 3, 4, 4, 1, 1, 1, 2, 1}, 
		          {1, 1, 2, 4, 4, 4, 4, 1, 1, 3}, 
		          {4, 2, 2, 4, 4, 3, 2, 4, 4, 1}, 
		          {1, 1, 2, 1, 2, 4, 4, 2, 4, 1}, 
		          {1, 2, 4, 3, 1, 4, 2, 2, 4, 3}, 
		          {4, 3, 4, 3, 2, 3, 2, 4, 4, 1}},
  100, 10^-4], 0.01]
	,
	{0.45, 0.55, {0.09, 0.27, 0.27, 0.38}, {0.37, 0.19, 0.12, 0.32}}
	,
	TestID->"Known Input Sample"
]

(* Check that it still works when the number of faces changes*)

Test[
	Round[diceEM[{{2, 3, 3, 4, 2, 5, 3, 3, 2, 4}, 
		          {3, 1, 3, 4, 4, 1, 1, 1, 2, 1}, 
		          {1, 1, 2, 4, 4, 4, 4, 1, 1, 3}, 
		          {4, 2, 2, 4, 4, 3, 2, 4, 4, 1}, 
		          {1, 1, 2, 1, 2, 4, 4, 2, 4, 1}, 
		          {1, 2, 4, 3, 1, 4, 2, 2, 4, 3}, 
		          {4, 3, 4, 3, 2, 3, 2, 4, 4, 1}},
  100, 10^-4], 0.01]
	,
	{0.14, 0.86, {0., 0.3, 0.4, 0.2, 0.1}, {0.28, 0.22, 0.15, 0.35, 0.}}
	,
	TestID->"Number of Faces 4->5"
]


(*NOTE: The following test is stochastic -- on very, very rare occasions it may fail legitimately. If it fails 
	two or three times in a row something is definitely wrong.*)
Test[
	Round[diceEM[diceSample[0.4, 0.6, {0.3, 0.2, 0.2, 0.2}, {0, 0.2, 0.2, 0.6}, 4000, 30], 100, 10^-4], 0.1]
	,
	{0.4,0.6,{0.3,0.2,0.2,0.2},{0.,0.2,0.2,0.6}}
	,
	TestID->"Stochastic 1"
]

(*NOTE: The following test is stochastic -- on very, very rare occasions it may fail legitimately. If it fails 
	two or three times in a row something is definitely wrong.*)
Test[
	Round[diceEM[diceSample[0.4, 0.6, {0.3, 0.2, 0.3, 0.2}, {0.1, 0.2, 0.2, 0.5}, 4000, 30], 100, 10^-4], 0.1]
	,
	{0.4,0.6,{0.3, 0.2, 0.3, 0.2},{0.1,0.2,0.2,0.5}}
	,
	TestID->"Stochastic 2"
]

(*NOTE: The following test is stochastic -- on very, very rare occasions it may fail legitimately. If it fails 
	two or three times in a row something is definitely wrong.*)
Test[
	Round[diceEM[diceSample[0.4, 0.6, {0.3, 0.3, 0.2, 0.2}, {0.2, 0.2, 0.2, 0.4}, 4000, 30], 100, 10^-4], 0.1]
	,
	{0.4,0.6,{0.3,0.3,0.2,0.2},{0.2,0.2,0.2,0.4}}
	,
	TestID->"Stochastic 3"
]

diceSample[numType1_, numType2_, type1_, type2_, draws_, rollsPerDraw_] := 
	Module[{totalDie, probType1, probType2, distDice, sides, dist, picks},
		totalDie  = numType1+numType2;
		probType1 = numType1/totalDie;
		probType2 = numType2/totalDie;
		distDice  = EmpiricalDistribution[{probType1, probType2}->{1,2}];
		
		sides = Range[Length[type1]];
		dist  = {EmpiricalDistribution[type1->sides], EmpiricalDistribution[type2->sides]};
		picks = RandomVariate[distDice, draws];
		
		Table[RandomVariate[dist[[picks[[i]]]],rollsPerDraw], {i,1,draws}]
	]