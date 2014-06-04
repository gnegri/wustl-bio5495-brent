(* Mathematica Test File *)

(* Testing the function dicePosterior, which calculates the posterior probability
   of each die type in the bag-of-dice problem. *)

(* Testing that the dimensions of the sample are correct. *)

Test[
	dicePosterior[{1, 1, 1, 1}, 0.5, 0.5, {1/4, 1/4, 1/4, 1/4}, {1/4, 1/4, 1/4, 1/4}]
	,
	0.5
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

(* Testing that the code prperly infers the number of faces on the dice from the probablity vectors.*)
Test[
	Round[dicePosterior[{1, 1, 1, 4, 1, 1, 1}, 1/3, 2/3, {0.1, 0.1, 0.1, 0.4, 0.1, 0.1, 0.1}, {1/7, 1/7, 1/7, 1/7, 1/7, 1/7, 1/7}], 
		0.01]
	,
	0.78
	,
	TestID->"ProbabilityTest-20130809-R5O9V9"
]