CSC344-A3
=========
This is the third assignment for Oswego's CSC 344 - Programming Languages class.  
The specification for the assignment can be found here:  
http://gee.cs.oswego.edu/dl/csc344/a3.html
The specification will also be documented below:
CSC344 Assignment 3 (Scala)

Write a Scala program that simplifies arithmetic integer expressions as in assignment 2, except:
Expressions are represented as trees. For example, instead of lisp (* (+ a b) (- a 1)), define classes allowing something like Times(Plus(Var("a"), Var("b")), Minus(Var("a"), Const(1))). Your program can create the trees used to test the program in any way you like.
Rather than using a binding list, prompt the user for bindings.
Print results in infix C/Java/Scala form. For example, the above prints as (a + b) * (a - 1)
