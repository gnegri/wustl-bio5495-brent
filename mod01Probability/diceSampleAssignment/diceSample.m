(*diceSample[numType1, numType2, type1, type2, draws, rollsPerDraw] simulates a process in which 
  dice are drawn at random from a bag, rolled a fixed number of times, and returned to the bag.
  numType1 and numType2 are the numbers of Type 1 and Type 2 dice in the bag, respectively. 
  type1 and type2 are lists giving the probabilities of rolling a 1, 2, ... on a die of Type 1
  and Type 2, respectively. The two must be the same length and that length indicates the number
  of faces on the dice. For example, if type1 were {1/4, 1/2, 1/4, 0} that would indicate that the 
  probability of rolling 1 is 1/4, the probability of rolling 2 is 1/2, 3 is 1/4, and 4 is zero (it is 
  never produced). draws is an integer indicating how many different times a die is drawn from the bag,
  rolled, and returned to the bag. rollsPerDraw is an integer indicating how many times each die 
  is rolled before it is returned to the bag.
  
  The return value is a matrix (list of lists). Each row (list) has length rollsPerDraw and contains
  integers representing the die faces shown on the rolls of one die. *)
diceSample[numType1_, numType2_, type1_, type2_, draws_, rollsPerDraw_] := 
	Module[{totalDie, probType1, probType2, distDice, sides, dist, picks},
	totalDie  = numType1+numType2;
	probType1 = numType1/totalDie;
	probType2 = numType2/totalDie;
	distDice = EmpiricalDistribution[{probType1, probType2}->{1,2}];
	
	sides = Range[Length[type1]];
	dist = {EmpiricalDistribution[type1->sides], EmpiricalDistribution[type2->sides]};
	
	picks = RandomVariate[distDice, draws];
	
	Table[RandomVariate[dist[[picks[[i]]]],rollsPerDraw], {i,1,draws}]
]