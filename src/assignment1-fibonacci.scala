package prg1.assignment1

case class QuadMatrix(element: Array[Array[BigInt]]) {
  def *(that: QuadMatrix): QuadMatrix = QuadMatrix({
    val a = this.element
    val b = that.element
    Array(Array(a(0)(0)*b(0)(0) + a(0)(1)*b(1)(0), a(0)(0)*b(0)(1) + a(0)(1)*b(1)(1)),
      Array(a(1)(0)*b(0)(0) + a(1)(1)*b(1)(0), a(1)(0)*b(1)(0) + a(1)(1)*b(1)(1)))
  })

  def pow(n: Int): QuadMatrix = {
    if (n == 0) QuadMatrix(Array(Array(BigInt(1), BigInt(0)), Array(BigInt(0), BigInt(1))))
    else if (n % 2 == 0) (this * this).pow(n / 2)
    else this * (this * this).pow((n - 1) / 2)
  }
}

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
    while (m >= 1) {
      c = a
      a += b
      b = c
      m = m - 1
    }
    b
  }

  //An implementation of the Fibonacci function using matrix products
  def fib_matrix(n: Int): BigInt = QuadMatrix(Array(Array(BigInt(1), BigInt(1)), Array(BigInt(1), BigInt(0)))).pow(n).element(1)(0)

  //An implementation of the Fibonacci function using polynomial products
  def fib_polynomial(n: Int): BigInt = {
    def multi(x: Array[BigInt], y:Array[BigInt]): Array[BigInt] = {
      val res = new Array[BigInt](x.length + y.length - 1)
      for (i <- res.indices) res(i) = 0
      for (i <- x.indices) for (j <- y.indices) res(i + j) += x(i) * y(j)
      res
    }

    def zip(x: Array[BigInt], isEven: Boolean): Array[BigInt] = {
      val res = new Array[BigInt]((x.length + 1) / 2)
      for (i <- res.indices) res(i) = x(2 * i + (if(isEven) 0 else 1))
      res
    }

    def multiZip(x: Array[BigInt], y:Array[BigInt], isEven: Boolean): Array[BigInt] = {
      val res = new Array[BigInt]((x.length + y.length) / 2)
      for (i <- res.indices) res(i) = 0
      for (i <- x.indices) for (j <- y.indices) if (((i + j) % 2 == 0) == isEven) res((i + j) / 2) += x(i) * y(j)
      res
    }

    def reverse(x: Array[BigInt]): Array[BigInt] = {
      val res = new Array[BigInt](x.length)
      for (i <- x.indices) res(i) = if (i % 2 == 0) x(i) else -x(i)
      res
    }
    
    def solve(p: Array[BigInt], q:Array[BigInt], m: Int): BigInt = {
      val rq = reverse(q)
      if (m == 0) p(0)
      else if (m % 2 == 0) solve(multiZip(p, rq, true), multiZip(q, rq, true), m / 2)
      else solve(multiZip(p, rq, false), multiZip(q, rq, true), (m - 1) / 2)
    }

    solve(Array(BigInt(0), BigInt(1)), Array(BigInt(1), BigInt(-1), BigInt(-1)), n)
  }

  def bench(f: Int => BigInt, n: Int, name: String): Unit = {
    val start = System.nanoTime()
    val r = f(n)
    val end = System.nanoTime()
    println(r)
    println(name + ": " + (" " * (20 - name.length)) + (end-start) + "ns")
  }

  def main(arg: Array[String]): Unit = {
    val n = 100

    //bench(fib_rec, n, "fib_rec")
    bench(fib_itr, n, "fib_itr")
    bench(fib_matrix, n, "fib_matrix")
    bench(fib_polynomial, n, "fib_polynomial")
  }
}
