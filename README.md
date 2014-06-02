scala_linearProg
================

#Approximate solution to linear programming problem by adjoining squared constraint violations to performance function. Parameter gamma (gamma > 0) sets balance between performance and constraint violation.  Smaller gamma results in smaller constraint violations.  The program is in scala worksheet.  Some examples and test cases are included.  The examples demonstrate that larger gamma gives better adherence to constraints.  For convergence of gradient descent steps on large problem it may be necessary to start with small gamma, and use resulting optimum values as starting condition for second (or more) passes with larger values of gamma.  
