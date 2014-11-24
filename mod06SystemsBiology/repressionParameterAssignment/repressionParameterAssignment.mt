(* Mathematica Test File *)

Clear[b,c,m]

Test[
	mutantEqns[{1, 1}]
	,
	{-1. + b[1] m[1](1 + c[1, 2] m[2]) == 0, -1. + b[2] (1 + c[2, 1] m[1]) m[2] == 0}
	,
	TestID->"repressionParameterAssignment-20131205-T5D7G1"
]

Test[
	mutantEqns[{1, 0}]
	,
	{-1. + b[1] m[1] == 0}
	,
	TestID->"repressionParameterAssignment-20131205-M4A5T9"
]

Test[
	expressionEqns[{-1.` + b[1] m[1] (1 + c[1, 1] m[1]) (1 + c[1, 2] m[2]) == 0, 
		            -1.` + b[2] (1 + c[2, 1] m[1]) m[2] (1 + c[2, 2] m[2]) == 0},
                   {{2, 3}, 
                    {{1, 2}, {3, 4}}}]
	,
	{-1. + 2 m[1] (1 + m[1]) (1 + 2 m[2]) == 0, 
	 -1. + 3 (1 + 3 m[1]) m[2] (1 + 4 m[2]) == 0}
	,
	TestID->"repressionParameterAssignment-20131205-H8O2B2"
]

Test[
	expressionMatrix[{{1}, {{1}}}, {{1}}]
	,
	{{1.}}
	,
	TestID->"repressionParameterAssignment-20131205-W9L4P5"
]

Test[
	expressionMatrix[{{1, 1}, {{1, 1}, {1, 1}}}, {{1, 1}}]
	,
	{{0.6180339887498948, 0.6180339887498949}}
	,
	TestID->"repressionParameterAssignment-20131206-O7B8D0"
]

Test[
	expressionMatrix[{{1, 1}, {{1, 1}, {1, 1}}}, 
		             {{1, 1}, {1, 0}, {0, 1}}]
	,
	{{0.6180339887498948, 0.6180339887498949}, {1., 0}, {0, 1.}}
	,
	TestID->"repressionParameterAssignment-20131206-C6G6X3"
]