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
		Module[{bVec=bVector, regsMask=regulatorsMask, gProfs=geneExpressionProfiles, gMasks=genePresenceMasks,
			nGenes=Length[regulatorsMask], nStrains=Length[geneExpressionProfiles], zeroSearch=Total[geneExpressionProfiles], 
			drops={}, mutantEqs, replaceB, replaceM, cMatrix, cVars, rpl},
		(*If the level of regulator j is 0 in all experiments then c[j,i] will never
		  appear in the equations and therefore can't be included in the list of variables
		  to solve for.*)
		Do[If[zeroSearch[[i]]==0, AppendTo[drops, i]],{i,nGenes}];
		{bVec,regsMask}=Drop[#, drops] &/@ {bVec,regsMask};
		{gProfs,gMasks}=Drop[#, None, drops] &/@ {gProfs,gMasks};
		nGenes=Length[regsMask];

		cMatrix = unknownCMatrix[nGenes,Table[1,{nGenes}],regsMask];
		cVars = Flatten[Reverse[Cases[#,Except[0]] &/@ cMatrix]];
		
		replaceB = b[#] -> bVec[[#]] &/@ Range[nGenes];
		replaceM = Table[m[#] -> gProfs[[i, #]] &/@ Range[nGenes], {i, nStrains}];
		mutantEqs = mutantEqns[#] &/@ gMasks /. replaceB;
		mutantEqs = Table[mutantEqs[[i]] /. replaceM[[i]],{i,nStrains}];
		mutantEqs = Cases[Flatten[mutantEqs],Except[False]];
		mutantEqs = parameterSumSquaredError[mutantEqs];
		
		rpl=NMinimize[mutantEqs,cVars];
		cMatrix/.rpl[[2]]
		]]

(* n: number of genes in system. everPresence: a binary vector containing 1 if a gene has a non-zero
   value in at least one of the experiments, 0 otherwise. regulatorsMask: a binary vector containing
   1 if a gene is a regulator and 0 otherwise.*)
unknownCMatrix[n_, everPresence_, regulatorsMask_]:=
	(Table[c[j,i], {j, 1, n}, {i, 1, n}]
		   * (1 - IdentityMatrix[n]) 
		   * everPresence * regulatorsMask)

(* parameterEqns replaces the symbolic mRNA expression levels of the generic equations with actual values
   from a gene expression matrix, returning a final set of equations with fixed mRNA levels and b parameters
   but variables for the c parameters.
parameterEqns[eqns_, bVector_, regulatorsMask_, geneExpressionProfile_]:= 
	Module[{},
	(*Sometimes the substitutions lead to an equation with no unknown variables in it. In that case
	  Mathematica evaluates it to True or False. We want to catch and eliminate those.*)
	]*)

(* Take the vector of equations with polynomials in m, c, and b on the left and zeros on the right and
   return the vector of left-hand-sides dotted with itself.*)
parameterSumSquaredError[eqns_]:=
	With[{errorVector=Map[#[[1]]&, eqns]},
		Dot[errorVector, errorVector]]
		
generateRandomParams[regulatorsMask_, seed_:"Grant Negri wrote this"]:= 
	Module[{bMatrix, cMatrix, n=Length[regulatorsMask]},
		SeedRandom[seed];
		bMatrix = Table[RandomReal[{0.01, 1}], {n}];
		cMatrix = Table[RandomReal[#] &/@ regulatorsMask, {n}];
		(* post-process *)
		Do[cMatrix[[i,i]] = 0, {i, n}];
		
		{bMatrix, cMatrix}
	]
	
(* mutantEqns takes a genePresenceMask indicating which genes were deleted in the cells 
   in which these mRNA measurements were taken. It generates a set of generic equations 
   and then replaces the mRNA level variables for the deleted genes with zeros, ensuring 
   that any actual measurments made on those deleted genes are ignored and any solver 
   doesn't have to deal with them.
*)
mutantEqns[genePresenceMask_]:= 
	Module[{activeGenes = Flatten[Position[genePresenceMask,1],1], n, interactions, activeParted, latterHalf},
		n = Length[activeGenes];
		(* hackish solution *)
		(* single case handling *)
		If[n==1, Return[{-1.0 + b[activeGenes[[1]]]m[activeGenes[[1]]]==0}]];
		(* get a list of all possible 1TF-1Target interactions *)
		interactions = Reverse[Partition[Permutations[activeGenes, {2}], n-1]];
		activeParted = Complement[activeGenes, #] &/@ Partition[activeGenes,1];
		(* fill out the product portion *)
		latterHalf = Times @@@ Flatten[1 + {c @@@ # &/@ interactions}*{m /@ # &/@ activeParted}, 1];

		Flatten[-1.0 + {b[#] m[#] &/@ activeGenes*latterHalf}, 1][[#]] == 0 &/@ Range[n]
	]

(* mutantExpVars returns a list of variables representing the expression levels of
   genes that remain in the genotype described by genePresenceMask. You will want 
   these later when you call FindRoot.*)
mutantExpVars[genePresenceMask_]:= m[#] &/@ Flatten[Position[genePresenceMask,1],1]
allExpVars[genePresenceMask_]:= m[#] &/@ Range[Length[genePresenceMask]]

(* expressionEqns replaces the symbolic parameters of the generic equations with actual values
   from a parameter matrix, returning a final set of equations with fixed parameters but variables
   for mRNA levels. Later, you will solve these for the mRNA variables yields to get a set of 
   expression levels that are consistent with the parameters.*)
expressionEqns[eqns_, {bVector_, cMatrix_}]:=
	Module[{n=Length[eqns]},
		Do[b[i] = bVector[[i]], {i, n}];
		Do[c[i, j] = cMatrix[[i, j]], {i, n}, {j, n}];
		eqns
	]

(* expressionMatrix takes parameters and a list of genePresenceMasks, one for each strain that we
   want an expression profile for. For each mask, it makes a set of equations and calls
   expressionProfile with those equations and the mask. The return value is a matrix in which 
   each row represents the expression level measurements for one strain (as described by one 
   genePresenceMask). Each column represents the expression of one gene in various strains. *)
expressionMatrix[params_, genePresenceMasks_]:=
	(*Make sure b, c, m are undefined here and in all functions called from here.*)
	Block[{b, c, m},
		Module[{aEV, mE, mEV, eqs, solveFor, replacementRules, replacementRules2, withM},
		aEV = allExpVars[#] &/@ genePresenceMasks;	
		
		mE  = mutantEqns[#] &/@ genePresenceMasks;
		eqs = expressionEqns[#, params] &/@ mE;
		
		mEV = mutantExpVars[#] &/@ genePresenceMasks;
		solveFor = Transpose[{#, Table[0, {Length[#]}]}] &/@ mEV;
		
		replacementRules = MapThread[FindRoot, {eqs, solveFor}];
		replacementRules2 = m[#] -> 0 &/@ Range[Length[aEV]];
		withM = aEV[[#]] /. replacementRules[[#]] &/@ Range[Length[aEV]];
		withM /. replacementRules2
	]]