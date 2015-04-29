# TMS-MAXIMA
Maxima compatible TMS code for step by step computation

I have been trying to learn about Truth Maintenance Systems and there are good explanations in "Building problem solvers". There is information available for all kinds of TMS. They have recreated SAINT integration program using TMS and they have also shown steps involved in reaching to logical conclusions from start to finish of the solution.

I have made changes to program to make it suitable for differentiation and integrated it into maxima and it is working fine for some set of rules that I have put in.

If you start maxima in the project directory this is fine.  If you start maxima outside the project directory you will need to add the path to the project to Maxima's internal paths as follows.

    /* Add the project location to Maxima's search path */
    file_search_maxima:append( [sconcat("/home/joker/Project Space/BPS/jtms/###.{mac,mc}")] , file_search_maxima)$
    file_search_lisp:append( [sconcat("/home/joker/Project Space/BPS/jtms/###.{lisp}")] , file_search_lisp)$

To use the code, for example try

    load("code-1.lisp");
    differentiate(x+x*y+x*z,x); 

Followed by
  
    explainresult();

This code is under development.  Some examples give wrong results or no results (denoted as FAILED) because all rules have not been put in to differentiate all types of expressions. Also, I have tried to format it but 'display' starts everything from a new line and it really breaks the format.

Possible improvements:
1) Including all the diff rules.
2) Better organized display.
3) Its also possible to specify the reason in words(any text or pattern) for why  a particular operation has been applied, I have removed it for now.
3) Using TMS to solve Integration, Limits and Simplification because they all require pattern matching.

Step-by-step explaination could be a good teaching tool for students and has been asked for many times. 

I am still working on adding rules to in jsops.lisp and testing various questions. Its like using integration table or differentiation table to match against and decide the next step saving the information on the way as nodes and justification for why some operation is valid.
