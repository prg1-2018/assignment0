package prg1.assignment1

object FIB {
  //An implementation of the Fibonacci function using the definition of the Fibonacci number
  def fib_rec(n: Int): BigInt = n match {
    case 0 | 1 => n
    case _ => fib_rec(n-1) + fib_rec(n-2)
  }

  //An implementation of the Fibonacci function using iteration (tail recursion)
  def fib_itr(n: Int): BigInt = {
    var a: BigInt = 1
    var b: BigInt = 0
    var temp: BigInt = 0
    for (m <- 1 to n) {
      temp = a + b
      b = a
      a = temp
    }
    b
  }

  //An implementation of the Fibonacci function using matrix products
  case class Matrix(a:BigInt, b:BigInt, c: BigInt, d: BigInt)
  def fib_matrix(n: Int): BigInt = {
    def productOf(mat1: Matrix, mat2: Matrix): Matrix = 
      Matrix(
          mat1.a*mat2.a+mat1.b*mat2.c,
          mat1.a*mat2.b+mat1.b*mat2.d,
          mat1.c*mat2.a+mat1.d*mat2.c,
          mat1.c*mat2.b+mat1.d*mat2.d
      )
    val a: Matrix = Matrix(1,1,1,0)
    val aPow2: Matrix = productOf(a,a)

    def pow(mat: Matrix, m: Int): Matrix = m match {
      case 0 => Matrix(1,0,0,1)
      case _ => {
        if (m%2 == 1) productOf(mat, pow(productOf(mat, mat), (m-1)/2))
        else pow(productOf(mat, mat), m/2)
      }
    }

    pow(a, n).c
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
