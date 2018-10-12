package prg1.assignment1

object FIB {
  //An implementation of the Fibonacci function using the definition of the Fibonacci number
  def fib_rec(n: Int): BigInt = n match {
    case 0 | 1 => n
    case _ => fib_rec(n-1) + fib_rec(n-2)
  }

  //An implementation of the Fibonacci function using iteration (tail recursion)
  def fib_itr(n: Int): BigInt = {
    var m : BigInt = n
    var a : BigInt = 1
    var b : BigInt = 0
    while (m >= 1){
        val c : BigInt = a
        a = a + b
        b = c
        m = m - 1
    }
    return (b)
  }

  //An implementation of the Fibonacci function using matrix products
  def fib_matrix(n: Int): BigInt = {
    def pow(a1:BigInt, a2:BigInt, b1:BigInt, b2:BigInt, n:Int):(BigInt, BigInt, BigInt, BigInt, Int) = 
        (a1, a2, b1, b2, n) match{
            case (a1, a2, b1, b2, 0) => (1, 0, 0, 1, 0)
            case (a1, a2, b1, b2, m) => if (m%2 == 1){
                pow(a1*a1+a2*b1, a1*a2+a2*b2, b1*a1+b2*b1, b1*a2+b2*b2, (m-1)/2) match{
                    case (aa1, aa2, bb1, bb2, l) => (aa1*a1+aa2*b1, aa1*a2+aa2*b2, bb1*a1+bb2*b1, bb1*a2+bb2*b2, (m-1)/2)
            }}
            else {
                pow(a1*a1+a2*b1, a1*a2+a2*b2, b1*a1+b2*b1, b1*a2+b2*b2, m/2)
            }
    }
    pow(1, 1, 1, 0, n) match{
        case (a, b, c, d, m) => c
    }
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
