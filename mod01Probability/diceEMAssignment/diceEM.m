 diceEM[sample_, maxIterations_, accuracy_]:=
	Module[{numFaces, binCounts, oldType1Prob, oldType2Prob, oldFaceProbs1, oldFaceProbs2, 
		    newType1Prob, newType2Prob, newFaceProbs1, newFaceProbs2},
		(* Initialize the local variables here.*)
		(* Loop here until either maxIterations has been reached or the sum of the absolute values of the changes from one 
		   iteration to the next in all estimated parameters is less than accuracy.
		   On each iteration, call updateProbs, passing in the old values, to set the new values. 
		   Then test whether the termination conditions have been met (Return[] breaks a loop returning its argument).
		   Finally, if termination conditions have not been met, set old values to be the same as the new values.
		   *)
		(*At the end, return the estimated parameters with the less likely die first.*)
		If[newType1Prob <= newType2Prob,
		   {newType1Prob, newType2Prob, newFaceProbs1, newFaceProbs2},
		   {newType2Prob, newType1Prob, newFaceProbs2, newFaceProbs1}]
	]

updateProbs[binCounts_, oldType1Prob_, oldType2Prob_, oldFaceProbs1_, oldFaceProbs2_] :=
	Module[{posteriors, type1Count, type2Count, faceCounts1, faceCounts2},
		(*Create list of posterior probabilities of a Type1 die having been rolled on each draw by calling your dicePosteriors,
		  which you should paste in to this file. *)
		(* Now use the posteriors to calculate EXPECTED counts for each die and each face in the sample.*)
		(* Finally, use these counts to compute maximum likelihood estimates for the parameters and return these estimates
		   in a list.*)
	]

(* Make sure to include your diceSample and dicePosterior functions here.*)
diceSample[numType1_, numType2_, type1_, type2_, draws_, rollsPerDraw_] := 
	Module[{totalDie, probType1, probType2, distDie, sides, dist1, dist2, picks, ans},
		totalDie  = numType1+numType2;
		probType1 = numType1/totalDie;
		probType2 = numType2/totalDie;
		distDie = EmpiricalDistribution[{probType1, probType2}->{1,2}];
		
		sides = Range[Length[type1]];
		dist1 = EmpiricalDistribution[type1->sides];
		dist2 = EmpiricalDistribution[type2->sides];
		
		picks = RandomVariate[distDie, draws];
		
		ans = { };
		
		For[i = 1, i <= draws, i++,
			If[picks[[i]]==1,
				AppendTo[ans, RandomVariate[dist1, rollsPerDraw]],
				AppendTo[ans, RandomVariate[dist2, rollsPerDraw]]
			]
		];
		
		ans
	]
	
dicePosterior[binCounts_, type1Prior_, type2Prior_, faceProbs1_, faceProbs2_] :=
	Module[{totalRolls, percentRolls, sides, actualRolls, pBgT1, pBgT2, pT1gB},
		totalRolls = Total[binCounts];
		percentRolls = binCounts/totalRolls;
		sides = Length[binCounts];
		
		actualRolls = { };
		For[i=1, i<=sides, i++,
			For[j=1, j<=binCounts[[i]], j++,
				AppendTo[actualRolls, i];
			];
		];
		
		pBgT1 = 1;
		pBgT2 = 1;
		For[k=1, k<=totalRolls, k++,
			pBgT1 *= faceProbs1[[actualRolls[[k]]]];
			pBgT2 *= faceProbs2[[actualRolls[[k]]]];
		];
		
		pT1gB = (pBgT1*type1Prior)/(pBgT1*type1Prior+pBgT2*type2Prior)
  	]
