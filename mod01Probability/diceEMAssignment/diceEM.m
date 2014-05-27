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