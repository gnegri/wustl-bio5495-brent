
(*A draw is the observed bases of a drawn sequence, which is subsequently replaced.
  sitePosterior calculates the posterior probability of a bound site versus a non-bound site, based on the
  observed sequence in the draw, the proportion of bound vs. non-bound sequences in the bag, and the 
  probabilities of observing each base in a sequence for bound and non-bound sites.
  The single number returned is the posterior probability of a bound site.*)
sitePosterior[sequence_, sitePrior_, backgroundPrior_, siteProbs_, backgroundProbs_] := 
 	Module[{len = Length[sequence],pSgBG,pSgS},
 		
 		(* sequence[[i]] is the base at that position; use that to get the 
 			probability of drawing that base from each distribution. *)
 		pSgBG = Product[backgroundProbs[[sequence[[i]]]],{i,len}];
 		pSgS  = Product[siteProbs[[i,sequence[[i]]]],{i,len}];
 		
 		(* zero event for both = .5; return the posterior probability otherwise *)
 		If[pSgBG == 0 && pSgS==0, .5, (sitePrior*pSgS)/(sitePrior*pSgS + backgroundPrior*pSgBG)]
 	]