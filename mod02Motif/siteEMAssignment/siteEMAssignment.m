 <<tools`
 
 siteEMFasta[inputFilename_, motifWidth_, maxIterations_, accuracy_]:=
 	Module[{sequences, siteProb, backgroundProb, siteFreqs, backgroundFreqs, lengths, numSeqs, subSubSeqs}, 
 		(* Read in the input file using readInput from tools,
 		   then use the sequences to produce a list of overlapping windows,
 		   each motifWidth long, to pass into siteEM *)
 		sequences=readInput[inputFilename, motifWidth];
 		lengths = Length[#] &/@sequences;
 		numSeqs = Length[sequences];
 		subSubSeqs = Flatten[Table[sequences[[i, j;;j+motifWidth-1]],{i,numSeqs},{j,lengths[[i]]-(motifWidth-1)}], 1];
 		
 		(* Call siteEM, then return the value for lambda (siteProb) and 
 		   the value for theta (siteFreqs) *)
 		{siteProb, backgroundProb, siteFreqs, backgroundFreqs} = siteEM[subSubSeqs, maxIterations, accuracy];
 		{siteProb, siteFreqs}
 	]
 
 siteEM[sequences_, maxIterations_, accuracy_]:=
	Module[{old, new, iter = 0,
		siteProbNew, backgroundProbNew, siteFreqsNew, backgroundFreqsNew, 
		siteProbOld, backgroundProbOld, siteFreqsOld, backgroundFreqsOld},
		
		(* Initialize the local variables here.*)
		siteProbOld = .2;
		backgroundProbOld = .8;
		siteFreqsOld = Table[Normalize[RandomInteger[{1,10},4], Total], {Length[sequences\[Transpose]]}];
		backgroundFreqsOld = {.2, .2, .3, .3};
		old = {siteProbOld, backgroundProbOld, siteFreqsOld, backgroundFreqsOld};
		
		(* Loop here until either maxIterations has been reached or the sum of the absolute values of the changes from one 
		   iteration to the next in all estimated parameters is less than accuracy.
		   On each iteration, call updateProbs, passing in the old values, to set the new values. 
		   Then test whether the termination conditions have been met (Return[] breaks a loop returning its argument).
		   Finally, if termination conditions have not been met, set old values to be the same as the new values.
		   *)
		While[True,
			iter++;
			new = updateProbs[sequences, siteProbOld, backgroundProbOld, siteFreqsOld, backgroundFreqsOld];
			{siteProbNew, backgroundProbNew, siteFreqsNew, backgroundFreqsNew} = new;
			If[iter>=maxIterations || Total[Abs[new-old], 3] <= accuracy,
				Return[{siteProbNew, backgroundProbNew, siteFreqsNew, backgroundFreqsNew}]
			];
			{siteProbOld, backgroundProbOld, siteFreqsOld, backgroundFreqsOld} = new;
			old = new;
		]
		
		(*Return the bound site parameters first.*)
	]

updateProbs[sequences_, siteProbOld_, backgroundProbOld_, siteFreqsOld_, backgroundFreqsOld_] := 
 	Module[{sitePosteriors, backgroundPosteriors, siteExpected, backgroundExpected, 
 			siteProbNew, backgroundProbNew, siteFreqsNew, backgroundFreqsNew},
 		
 		(*Create list of posterior probabilities of a bound site 
 		having been picked for each draw by calling your sitePosteriors,
  		which you should paste in to this file.*)
  		sitePosteriors = sitePosterior[#, siteProbOld, backgroundProbOld, siteFreqsOld, backgroundFreqsOld] &/@sequences;
  		backgroundPosteriors = 1-sitePosteriors;
  		
  		(* Now use the posteriors to calculate EXPECTED counts for each die
		   and each face in the sample. *)
		 (* Length[sample] = # of draws *)
		 (* Length[siteFreqs] = #nt *)
		siteExpected = (BinCounts[#, {1,5,1}] &/@(sequences\[Transpose])*Total[sitePosteriors])+1;
		backgroundExpected = (Total[(BinCounts[#, {1,5,1}] &/@sequences)*backgroundPosteriors])+1;
		
		(*Finally, use these counts to compute maximum likelihood estimates for the
		parameters and return these estimates in a list.*)
  		siteFreqsNew = #/Total[#] &/@siteExpected;
  		backgroundFreqsNew = backgroundExpected/Total[backgroundExpected];

		siteProbNew = Total[sitePosteriors]/Length[sequences];
		backgroundProbNew = Total[backgroundPosteriors]/Length[sequences];
		
  		{siteProbNew, backgroundProbNew, siteFreqsNew, backgroundFreqsNew}
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