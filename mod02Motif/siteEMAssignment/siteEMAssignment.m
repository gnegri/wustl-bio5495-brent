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