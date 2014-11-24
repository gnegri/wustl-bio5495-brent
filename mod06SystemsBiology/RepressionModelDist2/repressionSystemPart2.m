(* Mathematica package *)

(* inferCMatrix takes a vector of b parameters, a list of expression profiles, and a 
   matched list of genePresenceMasks, one for each strain that we have an expression 
   profile for. For each profile-mask pair, it makes a set of equations with the c parameters
   as unknown variables. It is important the the right-hand-side of each equation be zero.
   A vector of the left-hand-sides of these equations is then dotted with itself to make a
   sum of squared errors, where the error is the deviation from zero. NMminize is then used to 
   find values of the c[i,j] parameters that are consistent with the equations, or as close to
   consistent as possible. We must use NMiminize instead of NSolve because the number of 
   equations may be larger than the number of variables and NSolve won't accept over-determined 
   system of equations *)
  
inferCMatrix[bVector_, regulatorsMask_, geneExpressionProfiles_, genePresenceMasks_]:=
	(*Make sure b, c, m are undefined here and in all functions called from here.*)
	Block[{b, c, m},
		(*If the level of regulator j is 0 in all experiments then c[j,i] will never
		  appear in the equations and therefore can't be included in the list of variables
		  to solve for.*)
		(*Write code here.*)
	     		      ]

(* n: number of genes in system. everPresence: a binary vector containing 1 if a gene has a non-zero
   value in at least one of the experiments, 0 otherwise. regulatorsMask: a binaray vector containing
   1 if a gene is a regulator and 0 otherwise.*)
unknownCMatrix[n_, everPresence_, regulatorsMask_]:=
	(Table[c[j,i], {j, 1, n}, {i, 1, n}]
		   * (1 - IdentityMatrix[n]) 
		   * everPresence * regulatorsMask)

(* parameterEqns replaces the symbolic mRNA expression levels of the generic equations with actual values
   from a gene expression matrix, returning a final set of equations with fixed mRNA levels and b parameters
   but variables for the c parameters.*)
parameterEqns[eqns_, bVector_, regulatorsMask_, geneExpressionProfile_]:=
	(*Sometimes the substitutions lead to an equation with no unknown variables in it. In that case
	  Mathematica evaluates it to True or False. We want to catch and eliminate those.*)

(* Take the vector of equations with polynomials in m, c, and b on the left and zeros on the right and
   return the vector of left-hand-sides dotted with itself.*)
parameterSumSquaredError[eqns_]:=
	With[{errorVector=Map[#[[1]]&, eqns]},
		Dot[errorVector, errorVector]]