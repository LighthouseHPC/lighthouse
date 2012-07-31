#include <stdlib.h>
#include <stdio.h>
#include "memmodel_par.h"
#include <string.h>

//Calculates the number of misses for each level of cache or TLB
long long *
all_missesP (struct machine *inmachine, struct node *inloop)
{
  long long i, j;
  long long *ret;
  int caches = 0;
  struct machine* pointer;
  int levels = 1;
  pointer = inmachine;
  caches += inmachine->numcaches;
  //printf("inloop: %lld\n", inloop->its);
  //printf("inloop: %lld\n", inloop->children[0]->its);
  //printf("inloop: %lld\n", inloop->children[0]->children[0]->its);
  while(pointer->children != NULL) {
  //fprintf(stderr, "caches %d", caches);
    pointer = pointer->children;
    caches += pointer->numcaches;
    levels++;
  } 
  //fprintf(stderr, "caches %d", caches);
  ret = malloc (caches * (sizeof (long long)));
  for (i = 0; i < inloop->variables; i++)
    inloop->vars[i]->misses =
      malloc (caches * sizeof (long long));
  for (i = 0; i < caches; i++)
    for (j = 0; j < inloop->variables; j++)
      inloop->vars[j]->misses[i] = 0;

  pointer = inmachine;
  j = 0;
  //fprintf(stderr, "cost\n");
  while(pointer != NULL) {
  //fprintf(stderr, "j: %lld\n", j);
    for (i = 0; i < pointer->numcaches; i++) {
      if(strcmp(pointer->caches[i]->name, "REG") == 0) {
      ret[j] = mem_misses_structureP (inloop, pointer->caches[i]->size, j, 1, NULL, pointer->threads) / pointer->caches[i]->linesize;
    //  fprintf(stderr, "reg_misses: %lld\n", ret[j]);
  //fprintf(stderr, "misses all %lld\n", pointer->caches[i]->linesize);
      }
      else {
  //fprintf(stderr, "misses all %lld\n", pointer->caches[i]->linesize);
      //i = mem_misses_structureP (inloop, pointer->caches[i]->size, j, 1, NULL, pointer->threads) / inmachine->caches[i]->linesize;
  //fprintf(stderr, "misses all %lld\n", j);
      //fprintf(stderr, "reg_misses: %lld\n", ret[j]);
        ret[j] = mem_misses_structureP (inloop, pointer->caches[i]->size, j, 0, NULL, pointer->threads) / pointer->caches[i]->linesize;
//fprintf(stderr, "misses all %lld\n", pointer->caches[i]->size);
  //    fprintf(stderr, "reg_misses: %lld\n", ret[j]);
      }
      j++;
    }
    pointer = pointer->children;
  }
  //fprintf(stderr, "misses all %lld\n", ret[0]);
  /*for (i = 0; i < caches; i++)
    for (j = 0; j < inloop->variables; j++)
    {
      inloop->vars[j]->misses[i] =
	inloop->vars[j]->misses[i] / inmachine->caches[i]->linesize;
    //  //fprintf(stderr, "misses %s, %lld, %lld, %lld\n", inloop->vars[j]->name, i, j, inloop->vars[j]->misses[i]);
    }*/
  return ret;
}

//Finds the number of misses to a memory structure of size for loop inloop.
//This does not factor in line size.
long long
mem_misses_structureP (struct node *inloop, long long mem_structure_size,
		      long long structure, int regs, struct node* loop_above, int threads)
{
  long long working_set_size, var_reuse_distance;
  long long misses = 0; 
  long long loop_misses = 0;
  int i, j;  
  long long *reuses;
  int avail;
  int vars;
  int *in_reg;
  int count;
  loop_misses = 0;
  reuses = malloc(inloop->variables*sizeof(long long));
  for(i = 0; i < inloop->variables; i++)
    reuses[i] = 0;
  working_set_size = calc_working_setP(inloop, threads);
  if(regs && inloop->its != 0)
    working_set_size += wordsize * 1;
  //fprintf(stderr, "working_set_size: %lld\n", working_set_size);
	    //    //fprintf(stderr, "loop_misses: \n");
  if (working_set_size <= mem_structure_size)	//no misses in this loop
    return 0;
  else
    {				//calculate number of misses  
      for (i = 0; i < inloop->numchildren; i++)
	{
	  loop_misses +=
	    mem_misses_structureP (inloop->children[i], mem_structure_size,
				  structure, regs, inloop, threads);
//        inloop->vars[i]->misses[structure] += mem_misses_structureP(inloop->children[i], mem_structure_size, structure);
	  //fprintf(stderr, "loop_misses: %lld\n", loop_misses);
	}
      if (loop_misses == 0)
	{			//this loop first place for misses
	        //fprintf(stderr, "l \n");
	  if(inloop->its == 0) {
            if(regs == 1) { 
	      avail = mem_structure_size/wordsize;
              avail -= 1; //take out free reg for moving values through
	      for (i = 0; i < inloop->variables; i++) {
	        if(first_accessP(inloop, i) && avail != 0 && (!iterated_aboveP(loop_above, inloop->vars[i]->iterates, inloop->vars[i]->iterate) || !var_used_onceP(inloop, i))) {
                  avail--;
 		  reuses[i] = -1;
                }
                else if(find_previous_accessP(inloop, i) != -1){
  		  reuses[i] == -1;
		}
                else {
                  loop_misses+= wordsize;
	          inloop->vars[i]->misses[structure] += wordsize;
                }
              }
	       //fprintf(stderr, "avail %d \n", avail);
	      return loop_misses;
            }
	    for (i = 0; i < inloop->variables; i++) {

	        //fprintf(stderr, "l3 \n");
	      if(first_accessP(inloop, i)) {
	        //fprintf(stderr, "l4 \n");
                loop_misses += wordsize;
	        //fprintf(stderr, "l4 \n");
	        inloop->vars[i]->misses[structure] += wordsize;
	        //fprintf(stderr, "l4 \n");
	      }
	    }
	    return loop_misses;
	  }
	        //fprintf(stderr, "l2 \n");
	  for (i = 0; i < inloop->variables; i++)
	    {
              if(regs == 1 && i == 0) {
              avail = mem_structure_size/wordsize;
              avail -= 2; //take out free reg for moving values through and one for iterate
              for (j = 0; j < inloop->variables; j++) {                
	       //fprintf(stderr, "loopmisses %s %d \n", inloop->vars[j]->name, avail);
                if(first_accessP(inloop, j) && avail != 0 && (!var_iterated_overP(inloop, j) || !var_used_onceP(inloop, j))) {
                  avail--;
	       //fprintf(stderr, "loopmisses %s %lld \n", inloop->vars[j]->name, inloop->vars[j]->misses[3]);
		
                  reuses[j] = -1;
                }
                else if(find_previous_accessP(inloop, j) != -1){
	       //fprintf(stderr, "loopmisses2 %s %lld \n", inloop->vars[j]->name, inloop->vars[j]->misses[3]);
                  reuses[j] == -1;
                }
                //else {
                  //loop_misses+= wordsize;
                  //inloop->vars[j]->misses[structure] += wordsize;
                //}
              }

                for(j = 0; j < inloop->variables; j++)
 	          if(reuses[j] != -1)
                    reuses[j] = calc_reuse_distanceP(inloop, j, threads); 
	        avail = mem_structure_size/wordsize - 1; //how many regs minus the loop iterate
		//find_smallestP(inloop->variables, inloop, avail, reuses);
	      }
	        while(reuses[i] == -1 /*&& i < inloop->numchildren*/) {
	       //fprintf(stderr, "here %s %lld \n", inloop->vars[i]->name, inloop->vars[i]->misses[3]);
		  i++;
	        if(inloop->variables == i)
		  break;
		}
	        if(inloop->variables == i)
		  break;
	        //fprintf(stderr, "i: %d %lld\n", i, inloop->variables);
	      if (var_used_onceP (inloop, i))	//variable only occurs once might be able to replace this with a call to calculate reuse distance once that function is in good form and make this logic simplier but probably not since call to RD calc only probably should be made when we need a RD
		if (var_iterated_overP (inloop, i))
		  {		//if this loop iterates it only its size matters
		    loop_misses += calc_var_sizeP (inloop, inloop->vars[i], threads);
		//fprintf(stderr, "here: %lld", inloop->vars[i]->misses[structure]);
		    inloop->vars[i]->misses[structure] +=
		      calc_var_sizeP (inloop, inloop->vars[i], threads);
	       //fprintf(stderr, "here %s %lld \n", inloop->vars[i]->name, inloop->vars[i]->misses[3]);
		  }
		else		//if this loop doesn't this loop its count times its size if its RD is too big otherwise just its size
		if (calc_reuse_distanceP (inloop, i, threads) > mem_structure_size)
		  {
//                      //fprintf(stderr, "here: %lld\n", loop_misses);
		    loop_misses +=
		      inloop->its * calc_var_sizeP (inloop, inloop->vars[i], threads);
		    inloop->vars[i]->misses[structure] +=
		      inloop->its * calc_var_sizeP (inloop, inloop->vars[i], threads);
		  }
		else
		  {		//probably should never get here.
//                      //fprintf(stderr, "here2: %lld\n", loop_misses);
		    //loop_misses += calc_var_sizeP(inloop, inloop->vars[i]); might need this back
		  }
	      else
		{		//Need to be more careful when used more than once
		  if (first_accessP (inloop, i))	//first access is always a special case
		    if (var_always_iterated_overP (inloop, i))
		      {		//When always iterated over just size matters
			//            //fprintf(stderr, "always iterated size %lld\n", calc_var_sizeP(inloop, inloop->vars[i]));
			loop_misses +=
			  calc_var_sizeP (inloop, inloop->vars[i], threads);
			inloop->vars[i]->misses[structure] +=
			  calc_var_sizeP (inloop, inloop->vars[i], threads);
		      }
		    else
		      {
			if (var_iterated_overP (inloop, i))
			  {	//if this loop iterates it only its size matters
			    loop_misses +=
			      calc_var_sizeP (inloop, inloop->vars[i], threads);
			    inloop->vars[i]->misses[structure] +=
			      calc_var_sizeP (inloop, inloop->vars[i], threads);
			  }
			else if (calc_reuse_distanceP (inloop, i, threads) >
				 mem_structure_size)
			  {	//if this loop doesn't this loop its count times its size
			    loop_misses +=
			      inloop->its * calc_var_sizeP (inloop,
							   inloop->vars[i], threads);
			    inloop->vars[i]->misses[structure] +=
			      inloop->its * calc_var_sizeP (inloop,
							   inloop->vars[i], threads);
			  }
			else
			  {	//just its size matters
			    loop_misses +=
			      calc_var_sizeP (inloop, inloop->vars[i], threads);
			    inloop->vars[i]->misses[structure] +=
			      calc_var_sizeP (inloop, inloop->vars[i], threads);
			  }
		      }
		  else
		    {
		      //        //fprintf(stderr, "RD before misses: %lld\n", calc_reuse_distanceP(inloop, i));
		      if (calc_reuse_distanceP (inloop, i, threads) > mem_structure_size)	//if doesn't fit in cache from previous miss
			if (var_iterated_overP (inloop, i))
			  {	//if this loop iterates it only its size matters
			    loop_misses +=
			      calc_var_sizeP (inloop, inloop->vars[i], threads);
			    inloop->vars[i]->misses[structure] +=
			      calc_var_sizeP (inloop, inloop->vars[i], threads);
			  }
			else
			  {	//if this loop doesn't this loop its count times its size
			    loop_misses +=
			      inloop->its * calc_var_sizeP (inloop,
							   inloop->vars[i], threads);
			    inloop->vars[i]->misses[structure] +=
			      inloop->its * calc_var_sizeP (inloop,
							   inloop->vars[i], threads);
			  }
		    }
		}
	      // //fprintf(stderr, "loop_misses2: %lld\n", loop_misses);
	    }
	}
      else
	{			//there were misses in a previous loop
	  //fprintf(stderr, "variables: %lld\n", inloop->variables);
          if(regs == 1) {
	    in_reg = malloc(inloop->variables*sizeof(int));
	   //fprintf(stderr, "variables: %lld\n", inloop->variables);
            avail = mem_structure_size/wordsize - 1; //take out loop iterate;
            find_distinct_varsP(inloop, &vars);
	    //if(vars > avail) //if we need a free reg to move values through the mem heirarchy
            //above line might be better this is a good fix but not nessarly right there might not be a right just tradeoffs without a ton of information
	    count = 0;
	    for(j = 0; j < inloop->variables; j++) 
	      if((!var_used_onceP(inloop, j) || !var_iterated_overP(inloop, j)) && first_accessP(inloop, j))
		count++;
	    if(count > avail) //we need another free reg since everything that wants to fit within can't
	      avail--;
	    //fprintf(stderr, "bavail: %d\n", avail);
	    for(j = 0; j < inloop->variables; j++) {
              in_reg[j] = 0;
	   //fprintf(stderr, "misses: %lld\n", inloop->vars[j]->misses[structure]);
               if(!first_accessP(inloop, j) && (inloop->vars[j]->misses[structure] == wordsize || inloop->vars[j]->misses[structure] == 0)) {
	         if(avail == 0 && reuses[find_previous_accessP(inloop, j)] != -1) {
		   inloop->vars[j]->misses[structure] = 0;
                   in_reg[j] = 1; 
		   loop_misses--;
                   continue;
                 }
	    //fprintf(stderr, "bavail: %d\n", avail);
		 if(reuses[find_previous_accessP(inloop, j)] != -1) {
                   if(!in_reg[find_previous_accessP(inloop, j)])
		     avail--;
		   in_reg[j] = 1;
		   inloop->vars[j]->misses[structure] = 0;
		   loop_misses--;
 	//	   if(!var_iterated_overP(inloop, i))  
		 }
	  //fprintf(stderr, "aavail: %d\n", avail);
	       }
	    }
	  //fprintf(stderr, "variables: %lld\n", inloop->variables);
	    //fprintf(stderr, "bavail: %d\n", avail);
	  }
	  for (i = 0; i < inloop->variables; i++)
	    {
              if(regs && i)
	      //fprintf(stderr, "misses: %lld\n", inloop->vars[i-1]->misses[structure]);
	      if(regs == 1 && avail != 0 && !var_iterated_overP(inloop, i) && reuses[i] != -1 && (inloop->vars[i]->misses[structure] == wordsize || inloop->vars[i]->misses[structure] == 0)) {
		inloop->vars[i]->misses[structure] = 0;
		loop_misses--;
	        in_reg[i] = 1;
		//if(reuses[find_next_accessP(inloop, i)] == -1)
                if(!in_reg[find_previous_accessP(inloop, i)])
		  avail--;
	  //fprintf(stderr, "i avail: %d %d\n", i, avail);
		continue;
	      }
	  //fprintf(stderr, "avail: %d\n", avail);
	    //    //fprintf(stderr, "i: %d\n", i);
	      if (var_used_onceP (inloop, i)) {	//variable only occurs once might be able to replace this with a call to calculate reuse distance once that function is in good form and make this logic simplier but probably not since call to RD calc only probably should be made when we need a RD
		if (var_iterated_overP (inloop, i))
		  {		//if this loop iterates it only its size matters
                   if(regs) {
		    //fprintf(stderr, "i: %d\n", i);
                     
		   }
	  //fprintf(stderr, "variables2: %d\n", i);
 		    if(regs && in_reg[i])
		      continue;
                    if(inloop->vars[i]->misses[structure] == 0) {
		      loop_misses += calc_var_sizeP (inloop, inloop->vars[i], threads) - inloop->vars[i]->misses[structure];
		      inloop->vars[i]->misses[structure] =
		      calc_var_sizeP (inloop, inloop->vars[i], threads);
                    }
                    else {
                      loop_misses += (inloop->its-1) * inloop->vars[i]->misses[structure];//minus one since we don't want ot recount previous misses
                      inloop->vars[i]->misses[structure] *= (inloop->its) ;
	  //fprintf(stderr, "variables2: %d\n", i);
	            }
		 }
		else
		  {		//if this loop doesn't this loop its count times its size         
                    if(regs)
		      //fprintf(stderr, "iii: %d\n", in_reg[i]);
		    if(regs && in_reg[i])
		      continue;
                    if(inloop->vars[i]->misses[structure] == 0) {
		      loop_misses +=
		        (inloop->its-1) * calc_var_sizeP (inloop,
							 inloop->vars[i], threads);
		      inloop->vars[i]->misses[structure] =
		        (inloop->its) * calc_var_sizeP (inloop,
							 inloop->vars[i], threads);
                    }
                    else {
                       loop_misses += (inloop->its-1) * inloop->vars[i]->misses[structure];//minus one since we don't want ot recount previous misses
                      inloop->vars[i]->misses[structure] *= (inloop->its);
                    }
		  }
              }
	      else
		{
		  //fprintf(stderr, "i: %lld\n", i);
		  if (first_accessP (inloop, i)) {	//first access special case
		    if (var_always_iterated_overP (inloop, i))
		      {		//if always iterated over just size matters
			     // //fprintf(stderr, "always iterated size %lld\n", calc_var_sizeP(inloop, inloop->vars[i]));              
			loop_misses +=
			  calc_var_sizeP (inloop, inloop->vars[i], threads) - inloop->vars[i]->misses[structure];
			inloop->vars[i]->misses[structure] =
			  calc_var_sizeP (inloop, inloop->vars[i], threads);
		      }
		    else
		      {
			if (var_iterated_overP (inloop, i))
			  {	//if this loop iterates it only its size matters                
			    if (inloop->vars[i]->misses[structure] != 0) 
 			    {
			      loop_misses += (inloop->its - 1)*inloop->vars[i]->misses[structure];
//			        (calc_var_sizeP (inloop, inloop->vars[i])*(inloop->its-1)) / inloop->its;
			      inloop->vars[i]->misses[structure] *= (inloop->its);
//			        (calc_var_sizeP (inloop, inloop->vars[i])*(inloop->its-1)) / inloop->its;
			    }
			    else {
			      loop_misses +=
			        calc_var_sizeP (inloop, inloop->vars[i], threads);
			      inloop->vars[i]->misses[structure] =
			        calc_var_sizeP (inloop, inloop->vars[i], threads);
			    }
							
			  }
			else if (calc_reuse_distanceP (inloop, i, threads) >
				 mem_structure_size)
			  {	//if this loop doesn't this loop its count times its size                
		            if(regs && in_reg[i]) {
                              //printf("here\n");
  			      continue;
 				}
			    loop_misses += (inloop->its - 1) * inloop->vars[i]->misses[structure];
// calc_var_sizeP (inloop, inloop->vars[i]);	//minus one since we don't want ot recount previous misses
			    inloop->vars[i]->misses[structure] *= (inloop->its);
			      //(inloop->its - 1) * calc_var_sizeP (inloop,
				//				 inloop->
				//				 vars[i]);
			  }
			else
			  {
		            if(regs && in_reg[i]) {
                              //printf("here\n");
 			      continue;
 }
			    loop_misses +=
			      (inloop->its - 1) * inloop->vars[i]->misses[structure];
			    inloop->vars[i]->misses[structure] *= (inloop->its);
//* calc_var_sizeP (inloop,
//								 inloop->
//								 vars[i]);
			  }
		      }
		    }
		  else
		    {		//not first access
		      if(regs && in_reg[i])
		        continue;
		      var_reuse_distance = calc_reuse_distanceP (inloop, i, threads);	//might be able to merge into if statement
		       //fprintf(stderr, "var_reuse_distance: %lld\n", var_reuse_distance);
		      if (var_reuse_distance > mem_structure_size) {	//misses occur
			if (inloop->vars[i]->misses[structure] != 0)
			  {	//child has misses
			    loop_misses += (inloop->its - 1) * inloop->vars[i]->misses[structure];
//calc_var_sizeP (inloop->children[childP(inloop, i)], inloop->vars[i]);	//the misses from before are already counted
			    inloop->vars[i]->misses[structure] *= inloop->its;
			      //(inloop->its - 1) *
			      //calc_var_sizeP (inloop->
				//	     children[childP(inloop, i)],
				//	     inloop->vars[i]);
			  }
			else
			  {	//child doesn't have misses then size only matters
	      //fprintf(stderr, "interesting\n");
			    loop_misses +=
			      calc_var_sizeP (inloop, inloop->vars[i], threads);
			    inloop->vars[i]->misses[structure] +=
			      calc_var_sizeP (inloop, inloop->vars[i], threads);
			  }
		      }
		    }
		}
	      //fprintf(stderr, "loop_misses2: %lld\n", loop_misses);
	    }
	}
      misses += loop_misses;
      //fprintf(stderr, "misses: %lld\n", misses);
    }
  //fprintf(stderr, "misses2: %lld\n", misses);
  //if(regs == 1)
  //fprintf(stderr, "misses %lld \n", misses);
  return misses;
}

//Calculates the reuse distance of a variable for a given loop
long long
calc_reuse_distanceP (struct node *inloop, int location, int threads)
{
  int i, j;
  long long reuse = 0;
  int prev, last;
  char **iterates;
  long long *its;

  if (var_used_onceP (inloop, location))
    {				//wrong if not iterated over
      for (i = 0; i < inloop->numchildren; i++)	//figure out if the variable exists in a subloop    
	for (j = 0; j < inloop->children[i]->variables; j++)
	  if (strcmp
	      (inloop->children[i]->vars[j]->name,
	       inloop->vars[location]->name) == 0)
	    {			//if in a subloop find that reuse distance
	      reuse = calc_reuse_distanceP (inloop->children[i], j, threads);
	      if (var_iterated_overP (inloop, location))	//if iterated over
		{
//                      //fprintf(stderr, "return 1 %lld\n", calc_working_setP(inloop) - calc_working_setP(inloop->children[i]) + reuse);
		  return calc_working_setP(inloop, threads) -
		    calc_working_setP(inloop->children[i], threads) + reuse;
		}
	      else
		{		//find accessed in other loops
		  //          //fprintf(stderr, "return 2 %lld\n",  reuse);
		  reuse += reuse_other_loopsP (inloop, location, threads);	//function to find reuse not in subloop containing the variable
		  //        //fprintf(stderr, "return 2 %lld\n",  reuse);
		  return reuse;
		}
	    }
//                      //fprintf(stderr, "return 3 %lld\n", calc_working_setP(inloop));
      return calc_working_setP(inloop, threads);	//not in a subloop reuse distance = working set size
    }
  else
    {				//for both of these we need to make sure that variables are only counted once {

      //fprintf(stderr, "else %lld\n", reuse);
      if (first_accessP (inloop, location))
	{			//might need to pass in something else to know this
	  //fprintf(stderr, "else %lld\n", reuse);
	  last = find_last_accessP (inloop, location);
	  //fprintf(stderr, "child RD%d\n", childP(inloop, location));
	  for (i = 0; i < childP(inloop, location); i++)	//add on data in loops before loop with first access
	    reuse += calc_working_setP(inloop->children[i], threads);
	  //fprintf(stderr, "last %lld\n", reuse);
	  for (i = inloop->numchildren - 1; i > childP(inloop, last); i--)	//add on data from loops after loop with last access
	    reuse += calc_working_setP(inloop->children[i], threads);
	  //  //fprintf(stderr, "RD %lld\n", reuse);
	  //fprintf(stderr, "location %d\n", child_locationP(inloop,location));

	  reuse += before_varP (inloop->children[childP(inloop, location)], child_locationP (inloop, location), threads);	//add on part of first access loop
	  //fprintf(stderr, "RD %lld\n", reuse);
	  //All the below code is not needed  it doesn't matter if they're in a differnt subloop until you're in that loop
	  //if(in_different_subloopP(inloop, prev, location)) {  
	  //  iterates[0] = inloop->iterate;
	  //  its[0] = inloop->its;
	  //fprintf(stderr, "last %lld\n", last);
	  //fprintf(stderr, "last %d\n", child_locationP(inloop,last));
	  //  reuse += after_varP(inloop->children[childP(inloop, last)], child_locationP(inloop, last), iterates, its, 1); //add on part of last access loop
	  //}
	  //else
          its = malloc (100 * sizeof (long long));
          iterates = malloc (100 * sizeof (char*));
	  reuse += after_varP (inloop->children[childP(inloop, last)], child_locationP (inloop, last), iterates, its, 0, threads);	//add on part of last access loop
          free(its);
          free(iterates);
	  //fprintf(stderr, "RD %lld\n", reuse);
	}
      else
	{			//some variables can be counted more than once in here though this may not be the case anymore
	  prev = find_previous_accessP (inloop, location);
	  for (i = childP(inloop, prev) + 1; i < childP(inloop, location); i++)	//add on data in loops between loops with accesses
	    reuse += calc_working_setP(inloop->children[i], threads);
//        //fprintf(stderr, "RD prev %lld\n", reuse);
	  //All the below code is not needed  it doesn't matter if they're in a differnt subloop until you're in that loop
	  //  iterates[0] = inloop->iterate;
	  //its[0] = inloop->its;
	  //reuse += after_varP(inloop->children[childP(inloop, prev)], child_locationP(inloop, prev), iterates, its, 1); //add on part of last access loop
	  //fprintf(stderr, "RD after %lld\n", reuse);
	  //}
	  //else
	  if (in_different_subloopP (inloop, prev, location))
	    {			//if not in the same subloop need to add on data accessed before this variable is reached
	      reuse += before_varP (inloop->children[childP(inloop, location)], child_locationP (inloop, location), threads);	//add on part of first access loop
	      //  //fprintf(stderr, "RD before %lld\n", reuse);
	    }
	  //fprintf(stderr, "prev %d\n", prev);
          its = malloc (100 * sizeof (long long));
          iterates = malloc (100 * sizeof (char*));
	  reuse += after_varP (inloop->children[childP(inloop, prev)], child_locationP (inloop, prev), iterates, its, 0, threads);	//add on part of last access loop
          free(its);
          free(iterates);
	  //fprintf(stderr, "RD after %lld\n", reuse);
	}
    }
  //fprintf(stderr, "return 4 %lld\n", reuse);
  return reuse;
}

//Finds the reuse distance of a variable accessed only once between loop iterations while ignoring data accessed in the subloop that contains the variable.
long long
reuse_other_loopsP (struct node *inloop, int location, int threads){
  int i, j, k, l, found;
  int count = 0;
  long long reuse = 0;
  //fprintf(stderr, "reuse new: %lld\n", reuse);
  for (i = childP(inloop, location) + 1; i < inloop->numchildren; i++)
    {				//go over subloops after variables subloop
      //fprintf(stderr, "reuse new9: %lld\n", reuse);
      for (j = 0; j < inloop->children[i]->variables; j++)
	{			//go over variables in that subloop
	  //fprintf(stderr, "reuse new2: %lld\n", reuse);
	  found = 0;
	  for (k = childP(inloop, location); k <= i; k++)
	    {			//go over all subloops between subloop checking so far
	      //fprintf(stderr, "reuse new4: %lld\n", reuse);
	      if (i != k)
		{		//if not looking at a variable in same subloops
		  //fprintf(stderr, "reuse new6: %lld\n", reuse);
		  for (l = 0; l < inloop->children[k]->variables; l++)
		    {		//loop over all variables in subloop to be checked
		      //fprintf(stderr, "reuse new7: %lld\n", reuse);
		      //fprintf(stderr, "reuse new7: %d %d %d %d %s\n", i, j,k,l, inloop->children[k]->vars[l]->name);
		      if (strcmp (inloop->children[i]->vars[j]->name, inloop->children[k]->vars[l]->name) == 0)	//we found the variable
			found = 1;
		      //fprintf(stderr, "reuse new8: %lld\n", reuse);
		    }
		}
	      else
		{		//we're checking in the same subloop stop before the variable we're looking at
		  //fprintf(stderr, "reuse new5: %lld\n", reuse);
		  for (l = 0; l < j; l++)
		    if (strcmp (inloop->children[i]->vars[j]->name, inloop->children[k]->vars[l]->name) == 0)	//we found the variable
		      found = 1;
		}
	    }
	  //fprintf(stderr, "reuse new3: %lld\n", reuse);
	  if (found == 0)	//if the variable hasn't already been found add on its size to the reuse distance
	    reuse +=
	      calc_var_sizeP (inloop->children[i],
			     inloop->children[i]->vars[j], threads);
	}
    }
  //fprintf(stderr, "reuse new: %lld\n", reuse);
  for (i = 0; i < childP(inloop, location); i++)	//loops over sub loops before the variables subloop
    for (j = 0; j < inloop->children[i]->variables; j++)
      {				//loop over their variables
	found = 0;
	//fprintf(stderr, "reuse new 10: %lld\n", reuse);
	if (!var_iterated_overP (inloop, count))	//if the variable being looked at is not iterated what happened before matters
	  for (k = childP(inloop, location); k < inloop->numchildren; k++)	//check all the subloops after where the variable was found
	    for (l = 0; l < inloop->children[k]->variables; l++)	//check all their variables
	      if (strcmp (inloop->children[i]->vars[j]->name, inloop->children[k]->vars[l]->name) == 0)	//we found the variable
		found = 1;
	//fprintf(stderr, "reuse new 11: %lld\n", reuse);
	for (k = 0; k <= i; k++)	//in either case we need to check all subloops before the loop if they were iterated over though the new vector is all that matters now
	  if (i != k)
	    {			//if not in the same subloop
	      //fprintf(stderr, "reuse new 12: %lld\n", reuse);
	      for (l = 0; l < inloop->children[k]->variables; l++)	//loop over all the variables
		if (strcmp (inloop->children[i]->vars[j]->name, inloop->children[k]->vars[l]->name) == 0)	//we found the variable
		  found = 1;
	    }
	  else
	    {
	      //fprintf(stderr, "reuse new 13: %lld\n", reuse);
	      for (l = 0; l < j; l++)	//loop over only other ones in subloop before this one
		if (strcmp (inloop->children[i]->vars[j]->name, inloop->children[k]->vars[l]->name) == 0)	//we found the variable
		  found = 1;
	    }

	//fprintf(stderr, "reuse new 12: %lld\n", reuse);
	if (found == 0)		//if the variable hasn't already been found add on its size to the reuse distance
	  reuse +=
	    calc_var_sizeP (inloop->children[i], inloop->children[i]->vars[j], threads);
	count++;		//used to see if variable is iterated over by the loop
      }
  //fprintf(stderr, "reuse new: %lld\n", reuse);
  return reuse;
}

//calculates the size of the working set of all data accessed by loop.  This removes all duplicate accesses to data
long long
calc_working_setP(struct node *inloop, int threads)
{
  long long working_set_size = 0;
  int i;
  struct var **vars;
  int numvars = 0;
  if (inloop->its == 0)		//if I'm a statement base case
    {
      working_set_size = wordsize * inloop->variables;
      if(threads != 1)
        for(i = 0; i < inloop->variables; i++)
	  if(inloop->vars[i]->summed)
	    working_set_size += threads - 1;
      //fprintf(stderr, "working_set_size: %lld state\n", working_set_size);
    }
  else
    {				//If I'm a loop union all my children's variables and calculate their working set size
      //for (i = 0; i < inloop->numchildren; i++)	//union the variables
	vars = find_distinct_varsP (inloop, &numvars);
      //fprintf(stderr, "numvars: %d loop\n", numvars);
      for (i = 0; i < numvars; i++) {	//find working set size
	  working_set_size += calc_var_sizeP (inloop, vars[i], threads);
      }
      //fprintf(stderr, "working_set_size: %lld loop\n", working_set_size);
      free(vars);
    }
  return working_set_size;
}

//calculate the size of a variable accessed within a loop
long long
calc_var_sizeP (struct node *inloop, struct var *invar, int threads)
{
  long long varsize = 0;
  int i;
  long long temp;
  if (inloop->numchildren == 0)
    {				//Am I a statement that contains the variable
      for (i = 0; i < inloop->variables; i++)
	//if(invar == inloop->vars[i])
	if (strcmp (invar->name, inloop->vars[i]->name) == 0) {
	  if(inloop->vars[i]->summed)
	    return threads*wordsize;
	  else
	    return wordsize;
        }
      return 0;
    }
  for (i = 0; i < inloop->numchildren; i++)
    {				//find the largest size the variable is in the tree
      //This might not account of some loops accessing i, j and others accessing i, k however this is probably correct in almost all or all cases
      temp = calc_var_sizeP (inloop->children[i], invar, threads);
      if (temp > varsize)
	varsize = temp;
    }
  for (i = 0; i < invar->iterates; i++)	//Does this loop iterate over the variable
    //if(inloop->iterate  == invar->iterate[i]) {
    if (strcmp (inloop->iterate, invar->iterate[i]) == 0)
      {
	varsize *= inloop->its;
	break;
      }
  return varsize;
}

//find all the distinct variables within a loop
struct var **
find_distinct_varsP (struct node *inloop, int *numvars)
{
  int i, j;
  struct var **vars;
  *numvars = 0;
  vars = malloc (inloop->variables * sizeof (struct var *));
  if (inloop->variables > 0)
    {
      vars[0] = inloop->vars[0];
      (*numvars)++;
      //fprintf(stderr, "numvars if: %d\n", *numvars);
    }
  for (i = 1; i < inloop->variables; i++)
    {
      for (j = 0; j < *numvars; j++)
	if (strcmp (vars[j]->name, inloop->vars[i]->name) == 0)
	  break;
      if (j == *numvars)
	{
	  vars[j] = inloop->vars[i];
	  (*numvars)++;
	  //fprintf(stderr, "numvars for: %d\n", *numvars);
	}
    }
  //fprintf(stderr, "numvars here: %d\n", *numvars);
  return vars;
}

//Finds if variable in location occurs more than once in invar
int
var_used_onceP (struct node *inloop, int location)
{
  int i;
  for (i = 0; i < inloop->variables; i++)
    if (i != location)
      if (strcmp (inloop->vars[i]->name, inloop->vars[location]->name) == 0)
	return 0;
  return 1;
}

//Finds if variable in location is iterated over in the current loop
int
var_iterated_overP (struct node *inloop, int location)
{
  int i;
  for (i = 0; i < inloop->vars[location]->iterates; i++)	//find if current loop iterates
    if (strcmp (inloop->iterate, inloop->vars[location]->iterate[i]) == 0)
      return 1;
  return 0;
}

//Finds if variable in location is iterated over in all loops in which it exists
int
var_always_iterated_overP (struct node *inloop, int location)
{
  int i, j;
  //      //fprintf(stderr, "always\n");
  //    //fprintf(stderr, "alway %d\n", location);
  if (inloop->its == 0)
    return 1;
  if (!var_iterated_overP (inloop, location))
    return 0;
  //  //fprintf(stderr, "always 3\n");

  if (inloop->numchildren == 0)	//iterates and last loop
    return 1;
  //fprintf(stderr, "always 2\n");
  for (i = 0; i < inloop->numchildren; i++)	//figure out if the variable exists in a subloop
    for (j = 0; j < inloop->children[i]->variables; j++)
      //if(inloop->children[i]->vars[j] == inloop->vars[location])
      if (strcmp
	  (inloop->children[i]->vars[j]->name,
	   inloop->vars[location]->name) == 0)
	return (var_always_iterated_overP (inloop->children[i], j));	//find out if iterated in subloop
  return 1;			//loop where variable is introduced
}

//returns 1 if the variable at location is first accessed at location
int
first_accessP (struct node *inloop, int location)
{
  int i;
  for (i = 0; i < inloop->variables; i++)
    //if(inloop->vars[i] == inloop->vars[location])
    if (strcmp (inloop->vars[i]->name, inloop->vars[location]->name) == 0)
      if (i == location)
	return 1;
      else
	return 0;
  return -1;
}

//finds the location of the previous access to a variable in a loop location is the access we're finding the previous access of
int
find_previous_accessP (struct node *inloop, int location)
{
  int i;
  for (i = location - 1; i >= 0; i--)
    //if(inloop->vars[i] == inloop->vars[location])
    if (strcmp (inloop->vars[i]->name, inloop->vars[location]->name) == 0)
      return i;
  return -1;
}

//finds the location of the next access to a variable in a loop location is the access we're finding the previous nextof
int
find_next_accessP (struct node *inloop, int location)
{
  int i;
  for (i = location + 1; i < inloop->variables; i++)
    //if(inloop->vars[i] == inloop->vars[location])
    if (strcmp (inloop->vars[i]->name, inloop->vars[location]->name) == 0)
      return i;
  return -1;
}

//finds the location of the last access to a variable in a loop
int
find_last_accessP (struct node *inloop, int location)
{
  int i;
  for (i = inloop->variables - 1; i >= 0; i--)
    //if(inloop->vars[i] == inloop->vars[location])
    if (strcmp (inloop->vars[i]->name, inloop->vars[location]->name) == 0)
      return i;
  return -1;
}

//Finds which child within inloop the variable at location comes from
int
childP(struct node *inloop, int location)
{
  int i, j = 0;
  //fprintf(stderr, "child %lld\n", inloop->numchildren);
  for (i = 0; i < inloop->numchildren; i++)
    {
      j += inloop->children[i]->variables;
      //fprintf(stderr, "child j%d\n", j);
      if (j > location)
	return i;
    }
  return -1;
}

//Finds the location within the child loop of the variable
//Need to find the child in here and can't call the above function because if there are two references to the same variable in this loop we need to differentiate between the two
int
child_locationP (struct node *inloop, int location)
{
  int i, j, k = 0, l;
  for (i = 0; i < inloop->numchildren; i++)
    {
      l = 0;
      for (j = 0; j < inloop->children[i]->variables; j++)
	{
	  if (k == location)
	    return l;
	  k++;
	  l++;
	}
    }
  return -1;
}

//Calculate the amount of data accessed in a loop before a variable is accessed
//Neither this funtion or the next one accounts for a variable being found in between multiple times.
long long
before_varP (struct node *inloop, int location, int threads)
{
  long long reuse = 0;
  int i;
  //fprintf(stderr, "before var %d\n", location);
  //fprintf(stderr, "its %d\n", inloop->its);
  if (inloop->its == 0)
    return 0;
  for (i = 0; i < childP(inloop, location); i++)
    reuse += calc_working_setP(inloop->children[i], threads);
  reuse += before_varP (inloop->children[childP(inloop, location)], child_locationP (inloop, location), threads);	//call this again with loop and location of subloop containing location
  return reuse;
}

//This should work.  However if a variable is accessed more than once by a loop then like i,j,k loop struct but only i,k access pattern then the number will be wrong so it might be wrong for some vectors.  This must be kept in mind as testing.
long long
after_varP (struct node *inloop, int location, char **iterates, long long *its,
	   int num, int threads)
{
  long long reuse = 0;
  int i;
  //fprintf(stderr, "after var %d\n", location);
  //fprintf(stderr, "its %d\n", inloop->its);
  if (inloop->its == 0)
    {
      for (i = 0; i < inloop->variables; i++)
	reuse += calc_var_offitsP (inloop->vars[i], iterates, its, num, threads);	//need to find the size of the elements accesses with our variable
      return reuse;
    }
  //fprintf(stderr, "after var child %d %d\n", childP(inloop,location), childP(inloop, find_next_accessP(inloop, location)));
  //All the below code is not needed  it doesn't matter if they're in a differnt subloop until you're in that loop
  //if I'm used only once in this sub loop or the next access to me is in a diffent subloop
  if ( /*var_used_onceP(inloop, location) */ location ==
      find_last_accessP (inloop,
			location)
      /*|| in_different_subloopP(inloop, location, find_next_accessP(inloop, location)) */
      )
    {
      // //fprintf(stderr, "loc: %d\n", location);
      if (!var_used_onceP (inloop, location))
	{
	  location = used_in_sameP (inloop, location);
	}
      // //fprintf(stderr, "loc2: %d\n", location);
      iterates[num] = inloop->iterate;
      //fprintf(stderr, "loc3: %d\n", location);
      its[num] = inloop->its;
      //fprintf(stderr, "loc4: %d\n", location);
      num++;
      //fprintf(stderr, "loc5: %d\n", num);
      for (i = inloop->numchildren - 1; i > childP(inloop, location); i--)
	{

	  //fprintf(stderr, "loc6: %d\n", i);
	  //fprintf(stderr, "after var %d\n", location);
	  reuse +=
	    calc_working_set_plus_itsP (inloop->children[i], iterates, its,
				       num, threads);
	  //fprintf(stderr, "RD 1: %lld\n", reuse);
	}
    }
  reuse += after_varP (inloop->children[childP(inloop, location)], child_locationP (inloop, location), iterates, its, num, threads);	//add on part of last access loop
  //fprintf(stderr, "after var %d\n", location);
  //fprintf(stderr, "RD 2: %lld\n", reuse);
  return reuse;
}

//Takes in a location and returns an earlier location if the variable is used in the same inner loop at a previos loop.
int
used_in_sameP (struct node *inloop, int location)
{
  int i;
  //fprintf(stderr, "blah: \n" );
  //fprintf(stderr, "loc2: %d\n", inloop->children[childP(inloop, location)]->its);
  //fprintf(stderr, "loc2: %d\n", inloop->numchildren);
  if (inloop->children[childP(inloop, location)]->its == 0)	//if statement
    return first_accessP (inloop, location);
  if (inloop->numchildren == 1)	//if just one child move down the tree
    return used_in_sameP (inloop->children[0], location);
  if (var_used_onceP (inloop->children[childP(inloop, location)], child_locationP (inloop, location)) == 1)	//if new child unique
    return location;
  location =
    used_in_sameP (inloop->children[childP(inloop, location)],
		  child_locationP (inloop, location));
  for (i = 0; i < childP(inloop, location); i++)
    location += inloop->children[i]->variables;
  return location;
}

//Figures out if two variables found in the same loop are in the same or different subloops
int
in_different_subloopP (struct node *inloop, int location1, int location2)
{
  if (childP(inloop, location1) != childP(inloop, location2))
    return 1;
  return 0;
}

//Calculates the working set size of inloop knowing that its already been iterated by the variables in iterates
long long
calc_working_set_plus_itsP (struct node *inloop, char **iterates,
			   long long *its, int num, int threads)
{
  long long size = 0;
  int i;
  struct var **vars;
  int numvars;
  // //fprintf(stderr, "working_plus:\n");
  if (inloop->its == 0)		//if I'm a statement
    for (i = 0; i < inloop->variables; i++)
      size += calc_var_offitsP (inloop->vars[i], iterates, its, num, threads);	//need to find the size of the elements accesses with our variable
  else
    {				//I have sub loops
      //fprintf(stderr, "calc: %lld\n", size);
      //for (i = 0; i < inloop->numchildren; i++)	//union the variables
	vars = find_distinct_varsP (inloop, &numvars);
      for (i = 0; i < numvars; i++)	//find working set size
	size += calc_var_offitsP (vars[i], iterates, its, num, threads) * calc_var_sizeP (inloop, vars[i], threads);	//multiply already looked at loops by future loops
      free(vars);
    }
  return size;
}

//Takes in a variable and all the iterates over that variable and how many times they iterate(its) and figures out its size
long long
calc_var_offitsP (struct var *variable, char **iterates, long long *its,
		 int num, int threads)
{
  long long size = wordsize;
  int i, j;
  if(variable->summed)
    size *= threads;
  //fprintf(stderr, "calc: %d\n", num);
  for (i = 0; i < num; i++)
    for (j = 0; j < variable->iterates; j++)
      if (strcmp (variable->iterate[j], iterates[i]) == 0)
	{
	  size *= its[i];
	  //fprintf(stderr, "size: %lld\n", size);
	}
  return size;
}

void find_smallestP(long long vars, struct node* inloop, int avail, long long* reuses) {
  int i, j; 
  long long smallest;
  int place;
//  //fprintf(stderr, "loop_misses: \n");
  for(i = 0; i < avail; i++) {
	       // //fprintf(stderr, "loop_misses: \n");
    smallest = 99999999;
    place = -1;
    for(j = 0; j < vars; j++) {
      if(reuses[j] < smallest && reuses[j] != -1) {
        place = j;
	smallest = reuses[j];
      }
    }
	    //    //fprintf(stderr, "loop_misses: \n");
    reuses[place] = -1;
	      //  //fprintf(stderr, "loop_misses: \n");
    for(j = 0; j < vars; j++) {
	      //  //fprintf(stderr, "place %d \n", place);
      if(place != -1)
        if(strcmp((const char*)inloop->vars[place], (const char*)inloop->vars[j]) == 0 && !first_accessP(inloop, j)) {
	  reuses[j] = -1;
//	        //fprintf(stderr, "tis \n");
}
    }
  }
//	        //fprintf(stderr, "loop_misses: \n");
  return;
}

int iterated_aboveP(struct node* inloop, long long iterates, char **iterate) {
  int i;
  if(inloop == NULL || inloop->iterate == NULL)
    return 0;
  for (i = 0; i < iterates; i++)
    if(strcmp(inloop->iterate, iterate[i]) == 0)
      return 1;
  return 0;
}
