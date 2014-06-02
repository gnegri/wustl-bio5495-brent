
(*A draw is the series of rolls between the time a die is drawn from the bag and the time it is returned to the bag.
  dicePosterior calculates the posterior probability of a type1 versus type2 die, based the number of times each face
  appears in that draw and the relative numbers of Type 1 and Type 2 dice in the bag as well as the face probabilities
  for Type 1 and Type 2 dice. The single number returned is the posterior probability of Type 1.*)
dicePosterior[binCounts_, type1Prior_, type2Prior_, faceProbs1_, faceProbs2_] :=
	Module[{sides, pBgT1, pBgT2, pT1gB},
		sides = Length[binCounts];

		pBgT1 = Product[faceProbs1[[k]]^binCounts[[k]],{k,sides}];
		pBgT2 = Product[faceProbs2[[k]]^binCounts[[k]],{k,sides}];
		
		pT1gB = (pBgT1*type1Prior)/(pBgT1*type1Prior+pBgT2*type2Prior)
  	]
