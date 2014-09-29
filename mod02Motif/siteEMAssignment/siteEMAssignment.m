 <<tools`
 
 siteEMFasta[inputFilename_, motifWidth_, maxIterations_, accuracy_]:=
 	Module[{}, 
 		(* Read in the input file using readInput from tools,
 		   then use the sequences to produce a list of overlapping windows,
 		   each motifWidth long, to pass into siteEM *)
 		   
 		(* Call siteEM, then return the value for lambda (siteProb) and 
 		   the value for theta (siteFreqs) *)
 		]
 
 siteEM[sequences_, maxIterations_, accuracy_]:=
	Module[{},
		(* Initialize the local variables here.*)
		
		(* Loop here until either maxIterations has been reached or the sum of the absolute values of the changes from one 
		   iteration to the next in all estimated parameters is less than accuracy.
		   On each iteration, call updateProbs, passing in the old values, to set the new values. 
		   Then test whether the termination conditions have been met (Return[] breaks a loop returning its argument).
		   Finally, if termination conditions have not been met, set old values to be the same as the new values.
		   *)
		
		(*Return the bound site parameters first.*)
	]

updateProbs[sequences_, oldSiteProb_, oldBackgroundProb_, oldSiteFreqs_, oldBackgroundFreqs_] := 
 	Module[{},
 		(*Create list of posterior probabilities of a bound site 
 		having been picked for each draw by calling your sitePosteriors,
  		which you should paste in to this file.*)
  		
  		(*Now use the posteriors to calculate EXPECTED counts for each die
		 and each face in the sample.*)
		
		(*Finally, use these counts to compute maximum likelihood estimates for the
		parameters and return these estimates in a list.*)
  		
  ]
(* Make sure to include your siteSample and sitePosterior functions here.*)
siteSample[siteProb_, backgroundProb_, siteFreqs_, backgroundFreqs_, numDraws_] := 
	Module[{nucelotides=Range[4], seqDist, picks, dist, siteDist, backgroundDist, numBases = Length[siteFreqs]},
		(*Verifying data input is in the correct form*)
		If[Length[backgroundFreqs]!=4 || Total[backgroundFreqs]!=1, Return["Improper backgroundFreqs"]];
		If[Length[siteFreqs\[Transpose]]!=4 || Total[Total[siteFreqs\[Transpose]]]!=Length[siteFreqs], Return["Improper siteFreqs sublist[s]"]];
		
		(* map each probability to 1 or 2 *)
		seqDist = EmpiricalDistribution[{siteProb, backgroundProb}->{1,2}];
		(* then pick numDraws_ number of either site or background seqs based on their probabilty *)
		picks = Thread[RandomVariate[seqDist, numDraws]];
		
		(* map each nucelotide probability to the nucleotides (represented as 1,2,3,4) for each site *)
		siteDist = EmpiricalDistribution[#->nucelotides]&/@siteFreqs;	
		(* creates a list of {{siteDist_n, backgroundDist},...} *)
		backgroundDist = Table[EmpiricalDistribution[backgroundFreqs->nucelotides],{numBases}];
		dist = {siteDist,backgroundDist};
		
		(* for each pick, generate a sequence based on the specific nuceleotide frequency *)
		Table[RandomVariate[dist[[#,i]]],{i,numBases}]&/@picks
	]

sitePosterior[sequence_, sitePrior_, backgroundPrior_, siteProbs_, backgroundProbs_] := 
 	Module[{len = Length[sequence],pSgBG,pSgS},
 		
 		(* sequence[[i]] is the base at that position; use that to get the 
 			probability of drawing that base from each distribution. *)
 		pSgBG = Product[backgroundProbs[[sequence[[i]]]],{i,len}];
 		pSgS  = Product[siteProbs[[i,sequence[[i]]]],{i,len}];
 		
 		(* zero event for both = .5; return the posterior probability otherwise *)
 		If[pSgBG == 0 && pSgS==0, .5, (sitePrior*pSgS)/(sitePrior*pSgS + backgroundPrior*pSgBG)]
 	]