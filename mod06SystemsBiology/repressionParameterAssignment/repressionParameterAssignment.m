(* Mathematica Raw Program *)

(*NOTE: the functions list here are just the skeleton. You will probably want to 
 write other functions and call them to implement the functions listed here. *)

(* does what the function name implies. n genes, binary regulator list, random seed (optional) 
generateRandomParams[n_, regulatorsMask_, seed_:"Grant Negri wrote this"]:= 
	Module[{bMatrix, cMatrix},
		SeedRandom[seed];
		bMatrix = Table[RandomReal[{0.01, 1}],{n}];
		cMatrix = Table[RandomReal[#]&/@regulatorsMask,{n}];
		(* post-process *)
		Do[cMatrix[[i,i]]=0, {i,n}];
		
		{bMatrix, cMatrix}
	]
*)

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
		interactions = Partition[Permutations[activeGenes, {2}], n-1];
		activeParted = Complement[activeGenes, #] &/@ Partition[activeGenes,1];
		(* fill out the product portion *)
		latterHalf = Times @@@ Flatten[1 + {c @@@ # &/@ interactions}*{m /@ # &/@ activeParted}, 1];

		Flatten[-1.0 + {b[#] m[#] &/@ activeGenes*latterHalf}, 1][[#]] == 0 & /@ Range[n]
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
		Print[replacementRules];
		replacementRules2 = m[#] -> 0 &/@Range[Length[aEV]];
		withM = aEV[[#]] /. replacementRules[[#]] &/@ Range[Length[aEV]];
		withM /. replacementRules2
	]]