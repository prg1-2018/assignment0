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
      while(m > 0){
          a += b
          b = a - b
          m -= 1
      }
    return b
  }

  //An implementation of the Fibonacci function using matrix products
  case class Mat(a11: BigInt, a12: BigInt, a22: BigInt)
  def matpro(a: Mat, b: Mat): Mat = {
      val Mat(a11, a12, a22) = a
      val Mat(b11, b12, b22) = b
      return Mat(a11 * b11 + a12 * b12, a11 * b12 + a12 * b22,
            a12 * b12 + a22 * b22)
  }
  def matpwo(a: Mat, n: Int):Mat = {
      if(n == 0) return Mat(1,  0, 1)
      if(n / 2 * 2 == n) return matpwo(matpro(a,a), n / 2)//ｎは偶数
      else return matpro(a, matpwo(matpro(a,a), (n - 1) / 2))//nは奇数
  }
  def fib_matrix(n: Int): BigInt = {


    val a = Mat(1, 1, 0)
    return matpwo(a, n).a12
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
    val n = 200

    //bench(fib_rec, n, "fib_rec")
    bench(fib_itr, n, "fib_itr")
    bench(fib_matrix, n, "fib_matrix")
    bench(fib_polynomial, n, "fib_polynomial")
  }
}
