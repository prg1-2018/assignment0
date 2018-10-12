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

    def pow(mat: Matrix, m: Int): Matrix = m match {
      case 0 => Matrix(1,0,0,1)
      case _ => {
        if (m%2 == 1) productOf(mat, pow(productOf(mat, mat), (m-1)/2))
        else pow(productOf(mat, mat), m/2)
      }
    }

    pow(Matrix(1,1,1,0), n).c
  }

  //An implementation of the Fibonacci function using polynomial products
  type Polynomial = Array[BigInt]
  def fib_polynomial(n: Int): BigInt = {
    def invertSignOdds(p: Polynomial): Polynomial = {
      var res: Polynomial = Array.fill(p.size)(0)
      for (i <- 0 to p.size-1) {
        if (i % 2 == 1) res(i) = -p(i)
        else res(i) = p(i)
      }
      res
    }

    def productOf(p: Polynomial, q: Polynomial): Polynomial = {
      val degP = p.size-1
      val degQ = q.size-1
      var res: Polynomial = Array.fill(degP+degQ+1)(0)
      for (i <- 0 to degP) {
        for (j <- 0 to degQ) {
          res(i+j) += p(i)*q(j)
        }
      }
      res
    }

    def filterByIndex(p: Polynomial, condition: Int => Boolean): Polynomial = 
      for {
        (v, i) <- p.zipWithIndex
        if condition(i)
      } yield v

    def ePrime(p: Polynomial, q: Polynomial): Polynomial = {
      val pq: Polynomial = productOf(p, invertSignOdds(q))
      filterByIndex(pq, i => i % 2 == 0)
    }

    def oPrime(p: Polynomial, q: Polynomial): Polynomial = {
      val pq: Polynomial = productOf(p, invertSignOdds(q))
      filterByIndex(pq, i => i % 2 == 1)
    }

    def prime(p: Polynomial): Polynomial = 
      filterByIndex(productOf(p, invertSignOdds(p)), i => i%2==0)

    def recursive(m: Int, p: Polynomial, q: Polynomial): BigInt = {
      if (m == 0) p(0)
      else if (m % 2 == 0) recursive(m/2, ePrime(p, q), prime(q))
      else recursive((m-1)/2, oPrime(p, q), prime(q))
    }

    recursive(n, Array[BigInt](0, 1), Array[BigInt](1, -1, -1))
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
