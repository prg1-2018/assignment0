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
    while(m>=1){
      val l:BigInt = a
      a = a + b
      b = l
      m = m - 1
    }
    b
  }

  //An implementation of the Fibonacci function using matrix products
  def fib_matrix(n: Int): BigInt = {
    def mltmtrx(A: List[BigInt], B: List[BigInt]): List[BigInt] = {
      List(A(0)*B(0)+A(1)*B(2), A(0)*B(1)+A(1)*B(3),
      A(2)*B(0)+A(3)*B(2), A(2)*B(1)+A(3)*B(3))
    }
    def pow(A: List[BigInt], m: Int): List[BigInt] = {
      if(m==0) return List(1, 0, 0, 1)
      else{
        if(m%2==0) pow(mltmtrx(A, A), m/2)
        else mltmtrx(A, pow(mltmtrx(A, A), m/2))
      }
    }
    val A:List[BigInt] = pow(List(1, 1, 1, 0), n)
    A(2)
  }

  //An implementation of the Fibonacci function using polynomial products
  def fib_polynomial(n: Int): BigInt = {
    def fib_GnrtFunc(a: BigInt, b: BigInt, c: BigInt, d: BigInt, m: Int): BigInt = {
      /*
      P(x)=a+bx, Q(x)=1+cx+dx^2
      */
      if(m==0) return a
      else{
        val C:BigInt = 2*d - c*c
        val D:BigInt = d*d
        if(m%2==0){
          fib_GnrtFunc(a, a*d-b*c, C, D, m/2)
        }
        else{
          fib_GnrtFunc(b-a*c, b*d, C, D, m/2)
        }
      }
    }
    fib_GnrtFunc(0, 1, -1, -1, n)
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
