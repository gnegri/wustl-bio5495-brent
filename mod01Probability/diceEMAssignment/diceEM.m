 diceEM[sample_, maxIterations_, accuracy_]:=
	Module[{numFaces, sampleSize, binCounts, old, new, iter=0, return,
		oldType1Prob, oldType2Prob, oldFaceProbs1, oldFaceProbs2, 
		    newType1Prob, newType2Prob, newFaceProbs1, newFaceProbs2},
		    
		(* Initialize the local variables here.*)
		numFaces = Max[sample];
		sampleSize = Dimensions[sample][[1]];
		binCounts = Table[BinCounts[sample[[i]],{1,numFaces+1,1}],{i,1,sampleSize}];
		oldFaceProbs1 = Normalize[RandomInteger[{1,10}, numFaces], Total];
		oldFaceProbs2 = Normalize[RandomInteger[{1,10}, numFaces], Total];
		oldType1Prob  = .55;
		oldType2Prob  = .45;
		
		(* Loop here until either maxIterations has been reached or the sum of the absolute values of the changes from one 
		   iteration to the next in all estimated parameters is less than accuracy.
		   On each iteration, call updateProbs, passing in the old values, to set the new values. 
		   Then test whether the termination conditions have been met (Return[] breaks a loop returning its argument).
		   Finally, if termination conditions have not been met, set old values to be the same as the new values.
		 *)
		Catch[
			While[True,
				old = {oldType1Prob,oldType2Prob,oldFaceProbs1,oldFaceProbs2};
				new = updateProbs[binCounts,oldType1Prob,oldType2Prob,oldFaceProbs1,oldFaceProbs2];
				{newType1Prob, newType2Prob, newFaceProbs1, newFaceProbs2} = new;
				iter++;
				If[iter>maxIterations || Abs[old-new] < accuracy,
					Throw[
						If[newType1Prob <= newType2Prob,
		  					return = {newType1Prob, newType2Prob, newFaceProbs1, newFaceProbs2},
		   					return = {newType2Prob, newType1Prob, newFaceProbs2, newFaceProbs1}
						]
					],
					{oldType1Prob, oldType2Prob, oldFaceProbs1, oldFaceProbs2} = new;
				];
			];
		];
		
		return
		(*At the end, return the estimated parameters with the less likely die first.
		If[newType1Prob <= newType2Prob,
		   {newType1Prob, newType2Prob, newFaceProbs1, newFaceProbs2},
		   {newType2Prob, newType1Prob, newFaceProbs2, newFaceProbs1}
		]*)
	]

updateProbs[binCounts_, oldType1Prob_, oldType2Prob_, oldFaceProbs1_, oldFaceProbs2_] :=
	Module[{posteriorType1, posteriorType2, type1Count, type2Count, faceCounts1, faceCounts2, 
		draws, rollsPerDraw,
		newType1Prob, newType2Prob, newFaceProbs1, newFaceProbs2},
		
		(*Create list of posterior probabilities of a Type1 die having been rolled on each draw by calling your dicePosteriors,
		  which you should paste in to this file. *)
		
		draws = Dimensions[binCounts][[1]];
		posteriorType1 = Table[dicePosterior[binCounts[[i]],oldType1Prob,oldType2Prob,oldFaceProbs1,oldFaceProbs2],{i,1,draws}];
		posteriorType2 = 1 - posteriorType1;
		
		(* Now use the posteriors to calculate EXPECTED counts for each die and each face in the sample.*)
		rollsPerDraw = Dimensions[binCounts][[2]];
		type1Count   = rollsPerDraw*posteriorType1;
		type2Count   = rollsPerDraw*posteriorType2;
		faceCounts1  = rollsPerDraw*oldFaceProbs1;
		faceCounts2  = rollsPerDraw*oldFaceProbs2;
		
		(* Finally, use these counts to compute maximum likelihood estimates for the parameters 
			and return these estimates in a list.*)
		(* FIX ME *)
		newType1Prob  = Total[type1Count]/(rollsPerDraw*draws);
		newType2Prob  = Total[type2Count]/(rollsPerDraw*draws);
		newFaceProbs1 = type1Count/Total[type1Count];
		newFaceProbs2 = type2Count/Total[type2Count];
		
		{newType1Prob,newType2Prob,newFaceProbs1,newFaceProbs2}
	]

(* Make sure to include your diceSample and dicePosterior functions here.*)
dicePosterior[binCounts_, type1Prior_, type2Prior_, faceProbs1_, faceProbs2_] := 
	Module[{sides, pBgT1, pBgT2}, 

 	sides = Length[binCounts];
 	
 	pBgT1 = Product[expHelper[faceProbs1[[j]],binCounts[[j]]], {j, sides}];
	pBgT2 = Product[expHelper[faceProbs2[[k]],binCounts[[k]]], {k, sides}];
	
	(pBgT1*type1Prior)/(pBgT1*type1Prior + pBgT2*type2Prior)
]

expHelper[a_, b_] := If[a==0 && b==0, 1, a^b];

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