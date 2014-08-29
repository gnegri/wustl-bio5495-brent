(* Check that the correct number of Fibonacci numbers are returned *)
Test[
	Length[fibonacciSequence[10]]
	,
	11
	,
	TestID->"test1"
]

(* Check that correct Fibonacci numbers are returned *)
Test[
	fibonacciSequence[0]
	,
	{0}
	,
	TestID->"test2"
]
Test[
	fibonacciSequence[1]
	,
	{0, 1}
	,
	TestID->"test3"
]
Test[
	fibonacciSequence[15]
	,
	{0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610}
	,
	TestID->"test4"
]

(* Check that the correct number of ratios are returned *)
Test[
	Length[approximateGoldenRatio[10]]
	,
	9
	,
	TestID->"test4"
]

(* Check that correct ratios are returned *)
Test[
	approximateGoldenRatio[15]
	,
	{1.000000000, 2.000000000, 1.500000000, 1.666666667, 1.600000000, \
1.625000000, 1.615384615, 1.619047619, 1.617647059, 1.618181818, \
1.617977528, 1.618055556, 1.618025751, 1.618037135}
	,
	EquivalenceFunction -> (Apply[# < 0.001 &, Abs[#1-#2]] &)
	,
	TestID->"test6"
]