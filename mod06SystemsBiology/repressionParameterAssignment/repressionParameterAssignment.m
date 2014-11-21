(* Mathematica Raw Program *)

(*NOTE: the functions list here are just the skeleton. You will probably want to 
 write other functions and call them to implement the functions listed here. *)

generateRandomParams[n_, regulatorsMask_, seed_]:=
	(* Write code here *)


(* mutantEqns takes a genePresenceMask indicating which genes were deleted in the cells 
   in which these mRNA measurements were taken. It generates a set of generic equations 
   and then replaces the mRNA level variables for the deleted genes with zeros, ensuring 
   that any actual measurments made on those deleted genes are ignored and any solver 
   doesn't have to deal with them.
*)
mutantEqns[genePresenceMask_]:=
	(* Write code here *)

(* mutantExpVars returns a list of variables representing the expression levels of
   genes that remain in the genotype described by genePresenceMask. You will want 
   these later when you call FindRoot.*)
mutantExpVars[genePresenceMask_]:=
	(* Write code here *)

(* expressionEqns replaces the symbolic parameters of the generic equations with actual values
   from a parameter matrix, returning a final set of equations with fixed parameters but variables
   for mRNA levels. Later, you will solve these for the mRNA variables yields to get a set of 
   expression levels that are consistent with the parameters.*)
expressionEqns[eqns_, {bVector_, cMatrix_}]:=
	(* Write code here *)

(* expressionMatrix takes parameters and a list of genePresenceMasks, one for each strain that we
   want an expression profile for. For each mask, it makes a set of equations and calls
   expressionProfile with those equations and the mask. The return value is a matrix in which 
   each row represents the expression level measurements for one strain (as described by one 
   genePresenceMask). Each column represents the expression of one gene in various strains. *)
expressionMatrix[params_, genePresenceMasks_]:=
	(*Make sure b, c, m are undefined here and in all functions called from here.*)
	Block[{b, c, m},
	(* Write code here *)
	]