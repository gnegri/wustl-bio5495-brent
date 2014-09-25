(*siteSample[siteProb, backgroundProb, siteFreqs, backgroundFreqs, numDraws] 
	simulates a process in which sites (subsequences of a promoter) are drawn 
	at random from a bag, recorded, and returned to the bag.
  
  siteProb and backgroundProb are the probabilities of bound sequences 
  	and non-bound sequences, respectively. 
  
  siteFreqs is a list of lists giving the probabilities of seeing the bases 
  	A,C,G or T in each position in a bound sequence, and backgroundFreqs 
  	lists giving the probabilities of seeing A,C,G or T in an unbound sequence.
  
  backgroundFreqs must be of length 4, while siteFreqs can be of any length, 
  	with each of its sublists being of length 4. 
  
  numDraws is an integer indicating how many different times a sequence is 
  drawn from the bag, recorded, and returned to the bag.
  
  You will need to figure out how long each output sequence should be; 
  it will be equal to the length of siteFreqs.
  
  The return value is a list of lists. Each sublist has length equal to the length of
  siteFreqs and contains integers representing the bases in one drawn sequence. 
  *)
siteSample[siteProb_, backgroundProb_, siteFreqs_, backgroundFreqs_, numDraws_] := 
	Module[{nucelotides=Range[4], seqDist, picks, dist, siteDist, backgroundDist, numSites = Length[siteFreqs]},
		(*Verifying data input is in the correct form*)
		If[Length[backgroundFreqs]!=4 || Total[backgroundFreqs]!=1, Return["Improper backgroundFreqs"]];
		If[Length[siteFreqs\[Transpose]]!=4 || Total[Total[siteFreqs\[Transpose]]]!=Length[siteFreqs], Return["Improper siteFreqs sublist[s]"]];
		
		seqDist = EmpiricalDistribution[{siteProb, backgroundProb}->{1,2}];
		picks = Thread[RandomVariate[seqDist, numDraws]];
		
		siteDist = EmpiricalDistribution[#->nucelotides]&/@siteFreqs;
		backgroundDist = Table[EmpiricalDistribution[backgroundFreqs->nucelotides],{numSites}];
		dist = Transpose[{siteDist,backgroundDist}];
		
		Table[RandomVariate[dist[[i,picks[[j]]]]],{j,1,numDraws},{i,1,numSites}]
	]
	
toBases[sample_] := sample/.{1->"A",2->"C",3->"G",4->"T"};