package prg1.assignment1

object FIB {
  //An implementation of the Fibonacci function using the definition of the Fibonacci number
  def fib_rec(n: Int): BigInt = n match {
    case 0 | 1 => n
    case _ => fib_rec(n-1) + fib_rec(n-2)
  } 

  //An implementation of the Fibonacci function using iteration (tail recursion)
  def fib_itr(n: Int): BigInt = {
  	var m = n
  	var a : BigInt = 1
   	var b : BigInt = 0
   	var c : BigInt = 1
    while (m >= 1) {
    	c = a
    	a = c+b
   		b = c 
   	    m = m-1
   	}
   	return b
  }

  //An implementation of the Fibonacci function using matrix products
def fib_matrix(n: Int): BigInt = {
	def pow(A:List[BigInt] ,b: BigInt): List[BigInt] = {
		if(b == 0) List(1,0,0,1)
		else if(b%2 == 1) mlt(A,pow(mlt(A,A),(b-1)/2))
		//A*pow(A^2,(b-1)/2)
		else pow(mlt(A,A),b/2)
		//pow(A^2,b/2)
    }
	def mlt(A:List[BigInt], B:List[BigInt]): List[BigInt] = {
		val c0 : BigInt = A(0) * B(0) + A(1) * B(2)
		val c1 : BigInt = A(0) * B(1) + A(1) * B(3)
		val c2 : BigInt = A(2) * B(0) + A(3) * B(2)
		val c3 : BigInt = A(2) * B(1) + A(3) * B(3)
		return List(c0,c1,c2,c3)
	}
	pow(List(1,1,1,0),n)(2)
  }

  //An implementation of the Fibonacci function using polynomial products
  def fib_polynomial(n: Int): BigInt = {
    0
  }

  def bench(f: Int => BigInt, n: Int, name: String): Unit = {
    val start = System.nanoTime()
    val r = f(n)
    val end = System.nanoTime()
    println(r)
    println(name + ": " + (" " * (20 - name.length)) + (end-start) + "ns")
  }

  def main(arg: Array[String]): Unit = {
    val n = 10
    
    bench(fib_rec, n, "fib_rec")
    bench(fib_itr, n, "fib_itr")
    bench(fib_matrix, n, "fib_matrix")
    bench(fib_polynomial, n, "fib_polynomial")
  }
}
