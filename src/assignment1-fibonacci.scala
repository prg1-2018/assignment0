package prg1.assignment1

object FIB {
  //An implementation of the Fibonacci function using the definition of the Fibonacci number
  def fib_rec(n: Int): BigInt = n match {
    case 0 | 1 => n
    case _ => fib_rec(n-1) + fib_rec(n-2)
  }

  //An implementation of the Fibonacci function using iteration (tail recursion)
  def fib_itr(n: Int): BigInt = {
    0
  }

  //An implementation of the Fibonacci function using matrix products
  def fib_matrix(n: Int): BigInt = {
    0
  }

  def main(arg: Array[String]): Unit = {
    val n = 10

    {
      val start = System.nanoTime()
      val r = fib_rec(n)
      val end = System.nanoTime()
      println(r)
      println("fib_rec:    " + (end-start) + "ns")
    }
    {
      val start = System.nanoTime()
      val r = fib_itr(n)
      val end = System.nanoTime()
      println(r)
      println("fib_itr:    " + (end-start) + "ns")
    }
    {
      val start = System.nanoTime()
      val r = fib_matrix(n)
      val end = System.nanoTime()
      println(r)
      println("fib_matrix: " + (end-start) + "ns")
    }
  }
}
