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
  	var a:BigInt = 1
  	var b:BigInt = 0
  	var c:BigInt = 0
  	
  	while(m >=1){
  		c = a
  		a = a+b
  		b = c
  		m = m - 1
  	}
  	b
  }

  //An implementation of the Fibonacci function using matrix products
  def fib_matrix(n: Int): BigInt = {
  	
  	val matrixA = Array(1.0, 1.0, 1.0, 0.0)
  	
  	def kakeru(A:Array[Int], B: Array[Int]):Array[Int] = {
  	}
  	 
  	def pow(n:Int, A: Array[Int]):Array[Int] = {
  		n match{
  			case 0 => Array(1.0, 0.0, 1.0, 0.0)
  			case_  => {
  				if(n%2 = 1) pow() 
  		}
  	
    0
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
