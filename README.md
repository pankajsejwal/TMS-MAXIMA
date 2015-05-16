# TMS-MAXIMA
Maxima compatible TMS code for step by step computation

I have been trying to learn about Truth Maintenance Systems and there are good explanations in "Building problem solvers". There is information available for all kinds of TMS. They have recreated SAINT integration program using TMS and they have also shown steps involved in reaching to logical conclusions from start to finish of the solution.

I have made changes to program to make it suitable for differentiation, used Maxima as front end and it is working fine.

You will need to add the path to the project to Maxima's internal paths as follows. For examplem

    /* Add the project location to Maxima's search path */
    file_search_maxima:append( [sconcat("/home/joker/Project Space/BPS/jtms/###.{mac,mc}")] , file_search_maxima)$
    file_search_lisp:append( [sconcat("/home/joker/Project Space/BPS/jtms/###.{lisp}")] , file_search_lisp)$

To use the code, for example try

    load("diff-tms.mac");
    diff_steps(x+x*y+x*z,x); =>
    
    =>d(x*z,x)+d(x*y,x)+d(x,x)
    =>x*d(z,x)+d(x,x)*z+d(x*y,x)+d(x,x)
    =>x*d(z,x)+d(x,x)*z+x*d(y,x)+d(x,x)*y+d(x,x)
    =>d(x,x)*z+x*d(y,x)+d(x,x)*y+d(x,x)
    =>z+x*d(y,x)+y+1
    =>z+y+1
    
    diff_steps(x^2+sinh(x)*(asin(x)),x); => 
    =>d(asin(x)*sinh(x),x)+d(x^2,x)
    =>asin(x)*d(sinh(x),x)+sinh(x)*d(asin(x),x)+d(x^2,x)
    =>sinh(x)*d(asin(x),x)+d(x^2,x)+asin(x)*cosh(x)*d(x,x)
    =>d(x^2,x)+(sinh(x)*d(x,x))/sqrt(1-x^2)+asin(x)*cosh(x)*d(x,x)
    =>x^2*(2*d(log(x),x)+d(2,x)*log(x))+(sinh(x)*d(x,x))/sqrt(1-x^2)+asin(x)*cosh(x)*d(x,x)
    =>x^2*((2*d(x,x))/x+d(2,x)*log(x))+(sinh(x)*d(x,x))/sqrt(1-x^2)+asin(x)*cosh(x)*d(x,x)
    =>sinh(x)/sqrt(1-x^2)+x^2*(d(2,x)*log(x)+2/x)+asin(x)*cosh(x)
    =>sinh(x)/sqrt(1-x^2)+asin(x)*cosh(x)+2*x
    

Possible improvements:
1) Also mentioning which rule has been implemented, but for now it has been left as excercise for student.
2) Using TMS to solve Integration, Limits and Simplification because they all require pattern matching.

There are no known bugs and output is always same as Maxima's "diff" command. It includes all algebraic, trignometric, hyperbolic functions. Please try as many problems as you can andin case of errors please report it to me at pankajsejwal@gmail.com
