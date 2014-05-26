
(*A draw is the series of rolls between the time a die is drawn from the bag and the time it is returned to the bag.
  dicePosterior calculates the posterior probability of a type1 versus type2 die, based the number of times each face
  appears in that draw and the relative numbers of Type 1 and Type 2 dice in the bag as well as the face probabilities
  for Type 1 and Type 2 dice. The single number returned is the posterior probability of Type 1.*)
dicePosterior[binCounts_, type1Prior_, type2Prior_, faceProbs1_, faceProbs2_] :=
	Module[{totalRolls, percentRolls, sides, actualRolls, die1Data, die2Data, pBgT1, pBgT2, pT1gB},
		totalRolls = Total[binCounts];
		percentRolls = binCounts/totalRolls;
		sides = Length[binCounts];
		
		actualRolls = { };
		For[i=1, i<=sides, i++,
			For[j=1, j<=binCounts[[i]], j++,
				AppendTo[actualRolls, i];
			];
		];
		
		(* this is wrong
		die1Data = EmpiricalDistribution[faceProbs1->Range[sides]];
		die2Data = EmpiricalDistribution[faceProbs2->Range[sides]];
		
		
		pBgT1 = Probability[z == actualRolls, z \[Distributed] die1Data];
		pBgT2 = Probability[x == actualRolls, x \[Distributed] die2Data];
		*)
		
		pBgT1 = 1;
		pBgT2 = 1;
		For[k=1, k<=totalRolls, k++,
			pBgT1 *= faceProbs1[[actualRolls[[k]]]];
			pBgT2 *= faceProbs2[[actualRolls[[k]]]];
		];
		
		pT1gB = (pBgT1*type1Prior)/(pBgT1*type1Prior+pBgT2*type2Prior)
  	]
