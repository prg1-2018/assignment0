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
	var a: BigInt = 1
	var b: BigInt = 0
	var c: BigInt = 0
	while (m>=1){
		c = a
		a += b
		b = c
		m -= 1
	}
	b
  }

  //An implementation of the Fibonacci function using matrix products
  def fib_matrix(n: Int): BigInt = {
    def mult(A: Array[BigInt], B: Array[BigInt]): Array[BigInt] = {
    	Array(A(0)*B(0)+A(1)*B(2), A(0)*B(1)+A(1)*B(3), A(2)*B(0)+A(3)*B(2), A(2)*B(1)+A(3)*B(3))
    }
    def pow(A: Array[BigInt], n: Int): Array[BigInt] = n match {
        case 0 => Array(1, 0, 0, 1)
        case _ => if (n%2 == 1) mult(A, pow(mult(A, A), (n-1)/2)) else pow(mult(A, A), n/2)
    }
    pow(Array(1, 1, 1, 0), n)(2)
  }

  //An implementation of the Fibonacci function using polynomial products
  def fib_polynomial(n: Int): BigInt = {
    def poly(n: Int, P: Array[BigInt], Q: Array[BigInt]): BigInt = n match {
        case 0 => P(0)
        case _ => if (n%2 == 0) poly(n/2, Array(P(0)*Q(0), P(0)*Q(2)-P(1)*Q(1)), Array(Q(0)*Q(0), 2*Q(0)*Q(2)-Q(1)*Q(1), Q(2)*Q(2)))
        else poly((n-1)/2, Array(P(1)*Q(0)-P(0)*Q(1), P(1)*Q(2)), Array(Q(0)*Q(0), 2*Q(0)*Q(2)-Q(1)*Q(1), Q(2)*Q(2)))
    }
    poly(n, Array(0, 1), Array(1, -1, -1))
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
