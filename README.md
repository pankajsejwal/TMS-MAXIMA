# TMS-MAXIMA
Maxima compatible TMS code for step by step computation

I have been trying to learn about Truth Maintenance Systems and there are good explanations in "Building problem solvers". There is information available for all kinds of TMS. They have recreated SAINT integration program using TMS and they have also shown steps involved in reaching to logical conclusions from start to finish of the solution.

I have made changes to program to make it suitable for differentiation and integrated it into maxima and it is working fine for some set of rules that I have put in.
example,

  differentiate(x+x*y+x*z,x); => z+y+1
  
  explainresult();
   Solved the problem:
Differentiate(Derivative(x*z+x*y+x,x))
z+y+1
=>
Differentiate(Derivative(x,x))
1
Differentiate(Derivative(x*y,x))
y
Differentiate(Derivative(x*z,x))
z



 showaograph();    //shows and-or graph created during finding solution to the problem
 
 LEVEL=0
Differentiate(Derivative(x*z+x*y+x,x))
    SOLUTION=  
z+y+1
    
    Or subgoals:    
[Derivative(x*z+x*y+x,x)]
   LEVEL=1
Derivative(x*z+x*y+x,x)
    Solved in following steps
      LEVEL=2
Differentiate(Derivative(x*y,x))
    SOLUTION=  
y
    
    Or subgoals:    
[Derivative(x*y,x)]
      LEVEL=2
Differentiate(Derivative(x*z,x))
    SOLUTION=  
z
    
    Or subgoals:    
[Derivative(x*z,x)]
         LEVEL=3
Derivative(x*y,x)
    Solved in following steps
         LEVEL=3
Derivative(x*z,x)
    Solved in following steps
            LEVEL=4
Differentiate(Derivative(x*z,x))
    SOLUTION=  
z
    
    Or subgoals:    
[Derivative(x*z,x)]
               LEVEL=5
Derivative(x*z,x)
    Solved in following steps
                  LEVEL=6
Differentiate(Derivative(x,x))
    SOLUTION=  
1

If you try some examples you might get wrong results or no results(denoted as FAILED) because all rules have not been put in to differentiate all types of equations. Also, I have tried to format it but 'displa' starts every thing from new line and it really breaks the format.
Possible improvements:
1) Including all the diff rules.
2) Better organized display.
3) Its also possible to specify the reason in words(any text or pattern) for why  a particular operation has been applied, I have removed it for now.
3) Using TMS to solve Integration, Limits and Simplification because they all require pattern matching.

Step by step explaination could be a good teaching tool for students and has been asked for many times. 

I am still working on adding rules to in jsops.lisp and testing various questions. Its like using integration table or differentiation table to match against and decide the next step saving the information on the way as nodes and justification for why some operation is valid.
