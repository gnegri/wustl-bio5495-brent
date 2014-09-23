(*A draw is the series of rolls between the time a die is drawn from the bag and the time it is returned to the bag.
  dicePosterior calculates the posterior probability of a type1 versus type2 die, based the number of times each face
  appears in that draw and the relative numbers of Type 1 and Type 2 dice in the bag as well as the face probabilities
  for Type 1 and Type 2 dice. The single number returned is the posterior probability of Type 1.*)
dicePosterior[binCounts_, type1Prior_, type2Prior_, faceProbs1_, faceProbs2_] := 
	Module[{pBgT1, pBgT2}, 
		(* Take the Product of each particular (faceprob^bincount) *)
		{pBgT1, pBgT2} = Inner[expHelper, #, binCounts, Times] &/@ {faceProbs1, faceProbs2};
		
		(* formula for posterior likelihood *)
		(pBgT1*type1Prior)/(pBgT1*type1Prior + pBgT2*type2Prior)
	]
(* performs the exponentiation, setting 0^0 = 1 *)
expHelper[a_, b_] := If[a==0 && b==0, 1, a^b];