
(*A draw is the series of rolls between the time a die is drawn from the bag and the time it is returned to the bag.
  dicePosterior calculates the posterior probability of a type1 versus type2 die, based the number of times each face
  appears in that draw and the relative numbers of Type 1 and Type 2 dice in the bag as well as the face probabilities
  for Type 1 and Type 2 dice. The single number returned is the posterior probability of Type 1.*)
dicePosterior[binCounts_, type1Prior_, type2Prior_, faceProbs1_, faceProbs2_] :=
	Module[{},
		totalRolls = Total[binCounts];
		percentRolls = binCounts/totalRolls;
		sides = Range[Length[binCounts]];
		
		die1Data = EmpiricalDistribution[faceProbs1->sides];
		die2Data = EmpiricalDistribution[faceProbs2->sides];
		
		pBgT1 = Probability[z == percentRolls, z \[Distributed] die1Data];
		pBgT2 = Probability[x == percentRolls, x \[Distributed] die2Data];
		
		pT1gB = (pBgT1*type1Prior)/(pBgT1*type1Prior+pBgT2*type2Prior);
		
		pT1gB
  	]
