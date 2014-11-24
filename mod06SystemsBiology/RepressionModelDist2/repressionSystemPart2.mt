(* Mathematica Test File *)


(*IMPORTANT: The first part of the assignment in 2014 specified that generateRandomParams should take 3
  arguments starting with the number of genes. For this part you must change it so that the number of
  genes is inferred from the length of the regulators mask.*)
Test[
	generateRandomParams[{1, 1}, 1]
	,
	{{0.8192155952693605, 0.12030541501992409}, 
	 {{0., 0.18780314670602638}, {0.24136096745765045, 0.}}
	 }
	,
	TestID->"repressionParameterInference-20141124-K8L4B7"
]

(*IMPORTANT: c[i,j] should represent the efficiency of repressor i in repressing transcription of genes j.
  Furthermore, in any matrix representation, c[i,j] should appear in the ith row, jith column. Thus,
  each row contains the parameters for a single repressor acting on all the genes. *)
Test[
	mutantEqns[{1, 1}]
	,
	{-1. + b[1]*m[1]*(1 + c[2, 1]*m[2]) == 0, -1. + b[2]*(1 + c[1, 2]*m[1])*m[2] == 0}
	,
	TestID->"repressionParameterAssignment-20131205-T5D7G1"
]

params2={{0.819216, 0.120305}, {{0., 0.187803}, {0.241361, 0.}}}

allSinglesAndWTPresenceMatrix[n_]:=
	Join[ConstantArray[1,{1,n}],
		 ConstantArray[1, {n,n}] - IdentityMatrix[n]]

Test[
	expressionMatrix[params2, allSinglesAndWTPresenceMatrix[2]]
	,
	{{0.42722481788816985, 7.69481953603668}, 
	 {0, 8.312206475208844}, 
	 {1.220679283607742, 0}}
	,
	TestID->"repressionParameterInference-20141124-G4S9V9"
]

Test[
	inferCMatrix[{0.8192155952693605`, 0.12030541501992409`}, 
		         {1, 1}, 
		         {{0.427226, 7.69479}, 
		          {0, 8.31218}, {1.22068, 0}}, 
		         {{1, 1}, 
		          {1, 0}, 
		          {0, 1}}]
	,
	{{0, 0.18780344363414234}, 
	 {0.24136108289059935, 0}}
	,
	TestID->"repressionParameterInference-20141124-G3K9Q3"
]
		 
testCMatrixInference[n_, seed_]:=
	With[{regulatorsMask=ConstantArray[1, {n}],
		  genePresenceMasks=allSinglesAndWTPresenceMatrix[n]},
		With[{params=generateRandomParams[regulatorsMask, seed]},
			With[{expressionMatrix=expressionMatrix[params, genePresenceMasks]},
				Map[If[# >10^-4, #, 0] &,
					params[[2]] - 
					 inferCMatrix[params[[1]],
					 			  regulatorsMask,
					 			  expressionMatrix,
					 			  genePresenceMasks],
					  {2}]]]]

Test[testCMatrixInference[3, 7]
	,
	ConstantArray[0, {3,3}]
	,
	TestID->"repressionParameterInference-20141124-L7C8R8"
]

Test[testCMatrixInference[7, 13]
	,
	ConstantArray[0, {7,7}]
	,
	TestID->"repressionParameterInference-20141124-L7C8R8"
]