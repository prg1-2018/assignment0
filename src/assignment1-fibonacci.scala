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
    var c:BigInt = 1
    while(m >= 1){
      c = a
      a += b
      b = c
      m -= 1
    }
    b
  }

  //An implementation of the Fibonacci function using matrix products
  case class Matrix(a11: BigInt, a12: BigInt, a21: BigInt, a22: BigInt)
  def fib_matrix(n: Int): BigInt = {
    def pow(A: Matrix, n: Int): Matrix = {
      def mult(a: Matrix, b: Matrix): Matrix = {
        val Matrix(x, y, z, w) = a
        val Matrix(p, q, r, s) = b
        Matrix(x*p+y*r, x*q+y*s, z*p+w*r, z*q+w*s)
      }

      n match {
        case 0 => Matrix(1,0,0,1)
        case _ => {
          if(n%2 == 1) mult(A,pow(mult(A, A), (n-1)/2))
          else pow(mult(A, A), n/2)
        }
      }
    }

    pow(Matrix(1,1,1,0),n).a21
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
