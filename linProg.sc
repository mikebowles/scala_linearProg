
	
	/*problem statement find vector x that maximized linear performance measure given linear constraints.
	Form of the problem is fp vector of coefficients so that
	
	performance = fp dot x.   ("fp dot x" means dot product of the two vectors fp and x)
	
	Constraints are described by a list of coefficient vectors fci and corresponding list of constants ci
	so the ith constraint is of the form
	
	fci dot x + ci  <= 0
	
	Solution approach is to turn problem into a sequence of unconstrained problems.  Choose a parameter
	gamma > 0 and augment the performance measure by
	
	augmented performance  = fp dot x - gamma * 1/2 (sum{ (fci dot x) + ci }^2
	
	where the sum is only over the constraints that are in violation.  Solve this problem by gradient
	descent.
	
	Start with a value of the constraint parameter gamma that is close to zero, converge the gradient descent
	then increase and repeat gradient descent until desired degree of constraint observance is attained
	
	Gradient is given by
	
	gradient of augmented performance = fp - gamma* ( sum{((fci dot x) +ci) * fci}
	
	where the sum only includes the constraints that are in violation.
	
	
	*/

object linProg {
	
	/** Define some utility functions to treat lists like vectors*/
	/*Add two lists as vectors*/
	def sumList(a: List[Double], b: List[Double]): List[Double] = {
		if(a.isEmpty) b
		else if(b.isEmpty) a
		else if(a.isEmpty && b.isEmpty) List()
		else (a.head + b.head)::sumList(a.tail, b.tail)
		
	}                                         //> sumList: (a: List[Double], b: List[Double])List[Double]
	/*Take dot product*/
	def dot(x: List[Double], y: List[Double]): Double = {
		require(checkDim(x) == checkDim(y), "in dot(x,y) x and y have different dimensions")
		if(x.isEmpty || y.isEmpty) 0
		else x.head*y.head + dot(x.tail, y.tail)
	}                                         //> dot: (x: List[Double], y: List[Double])Double
	
	/*multiply a list times a scalar*/
	def multList(a: List[Double], b: Double): List[Double] = {
		if(a.isEmpty) List()
		else b*a.head::multList(a.tail, b)
	}                                         //> multList: (a: List[Double], b: Double)List[Double]
	
	/*sum a list of lists (for summing constraint list)*/
	def partialSum(b: List[List[Double]]): List[Double] = {
		if(b.tail.isEmpty) b.head
		else sumList(b.head, partialSum(b.tail))
	}                                         //> partialSum: (b: List[List[Double]])List[Double]
/*
	def constraintEval(fc: List[List[Double]], c: List[Double], x: List[Double]): List[Double] = {
		println("y1:= " + fc)
		if(fc.isEmpty) List()
		/*check constraints for violation*/
		else if(dot(fc.head, x) + c.head <= 0.0) 0.0::constraintEval(fc.tail, c.tail, x)
		else 1.0::constraintEval(fc.tail, c.tail, x)
	}
*/
	
	def checkDim[T](f: List[T]): Int = {
		if(f.isEmpty) 0
		else 1 + checkDim(f.tail)
	}                                         //> checkDim: [T](f: List[T])Int
		
		
	
	
	/*test cases*/
	var x = List(4.0, 5.0, 6.0)               //> x  : List[Double] = List(4.0, 5.0, 6.0)
	val fc = List(List(1.0, 2.0, 3.0), List(2.0, 3.0, 4.0))
                                                  //> fc  : List[List[Double]] = List(List(1.0, 2.0, 3.0), List(2.0, 3.0, 4.0))
	val c = List(1.0, 2.0)                    //> c  : List[Double] = List(1.0, 2.0)
	
	//var constraintViol = constraintEval(fc, c, x)
  checkDim(x)                                     //> res0: Int = 3
	checkDim(fc)                              //> res1: Int = 2
	
	def constraintGradient(fc: List[List[Double]], c: List[Double], x: List[Double]): List[Double] = {
		if(fc.isEmpty) List()
		else {
			val a2 = dot(fc.head, x) + c.head
			//println("constraint value  " + a2)
			//println("fc.head  " + fc.head)
			if(a2 > 0) {
				val a1 = multList(fc.head, a2)
				//println("grad vect  " + a1)
				sumList(a1, constraintGradient(fc.tail, c.tail, x))
			}
			else{
				val a1 = multList(fc.head, 0.0)
				//println("grad vect  " + a1)
				sumList(a1, constraintGradient(fc.tail, c.tail, x))
			}
		}
	}                                         //> constraintGradient: (fc: List[List[Double]], c: List[Double], x: List[Doubl
                                                  //| e])List[Double]
	
	val cgTestFc = List(List(1.0, 0.0), List(0.0, 1.0))
                                                  //> cgTestFc  : List[List[Double]] = List(List(1.0, 0.0), List(0.0, 1.0))
  val cgTestc = List(0.0, 0.0)                    //> cgTestc  : List[Double] = List(0.0, 0.0)
	x = List(2.0, 0.0)
	
	constraintGradient(cgTestFc, cgTestc, x)  //> res2: List[Double] = List(2.0, 0.0)
	
	x = List(0.0, 2.0)
	
	constraintGradient(cgTestFc, cgTestc, x)  //> res3: List[Double] = List(0.0, 2.0)
	
	
	
	
	
	
	def gradDescent(fp: List[Double], fc: List[List[Double]], c: List[Double],
	guess: List[Double], eps: Double, gamma: Double): List[Double] = {
		val grad = sumList(fp, multList(constraintGradient(fc, c, guess), -gamma))
		val small = 0.001
		val newGuess = sumList(guess, multList(grad, eps))
		//println("grad: " + grad)
		//println("guess and newGuess: " + guess + newGuess)
		if(dot(grad, grad) < small) newGuess
		else {
			gradDescent(fp, fc, c, newGuess, eps, gamma)
		}
	}                                         //> gradDescent: (fp: List[Double], fc: List[List[Double]], c: List[Double], gu
                                                  //| ess: List[Double], eps: Double, gamma: Double)List[Double]
	var gamma = 1.0                           //> gamma  : Double = 1.0
	val guess = List(0.0, 0.0)                //> guess  : List[Double] = List(0.0, 0.0)
	val fc1 = List(List(1.0, 0.0), List(0.0, 1.0))
                                                  //> fc1  : List[List[Double]] = List(List(1.0, 0.0), List(0.0, 1.0))
	val c1 = List(0.0, 0.0)                   //> c1  : List[Double] = List(0.0, 0.0)
	val eps1 = 0.1                            //> eps1  : Double = 0.1
	val fp1 = List(1.0, 1.0)                  //> fp1  : List[Double] = List(1.0, 1.0)
	
	gradDescent(fp1, fc1, c1, guess, eps1, gamma)
                                                  //> res4: List[Double] = List(0.9817519963685992, 0.9817519963685992)
	var gamma2 = 10.0                         //> gamma2  : Double = 10.0
	val guess2 = List(0.0, 0.0)               //> guess2  : List[Double] = List(0.0, 0.0)
	val fc2 = List(List(1.0, 0.0), List(0.0, 1.0))
                                                  //> fc2  : List[List[Double]] = List(List(1.0, 0.0), List(0.0, 1.0))
	val c2 = List(0.0, 0.0)                   //> c2  : List[Double] = List(0.0, 0.0)
	val eps2 = 0.1                            //> eps2  : Double = 0.1
	val fp2 = List(1.0, 1.0)                  //> fp2  : List[Double] = List(1.0, 1.0)
	
	gradDescent(fp2, fc2, c2, guess2, eps2, gamma2)
                                                  //> res5: List[Double] = List(0.1, 0.1)
	
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  //check out map, foldleft and foldright in scala
}