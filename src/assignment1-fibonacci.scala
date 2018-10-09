package prg1.assignment1

object FIB {
  //An implementation of the Fibonacci function using the definition of the Fibonacci number
  def fib_rec(n: Int): BigInt = n match {
    case 0 | 1 => n
    case _ => fib_rec(n-1) + fib_rec(n-2)
  }

  //An implementation of the Fibonacci function using iteration (tail recursion)
  def fib_itr(n: Int): BigInt = {
    var m = n// NOTE:
    var a:BigInt = 1
    var b:BigInt = 0
    var c:BigInt = 0
    while(m >= 1){
      c = b
      b = a
      a = a + c
      m = m - 1
    }
    b
  }

  //An implementation of the Fibonacci function using matrix products
  def fib_matrix(n: Int): BigInt = {

    def pow(l: List[BigInt], m: Int): List[BigInt] = {

      def squ(ls: List[BigInt]): List[BigInt] = ls match{
        case aa::ab::ba::bb::Nil => (aa*aa + ab*ba)::(aa*ab + ab*bb)::(ba*aa + bb*ab)::(ba*ab + bb*bb)::Nil
      }

      if(m == 0){
        List(1, 0, 0, 1)
      }else if(m % 2 == 1){
        val List(aa, ab, ba, bb) = pow(squ(l), (m-1)/2)
        val List(cc, cd, dc, dd) = l
        (cc*aa + cd*ba)::(cc*ab + cd*bb)::(dc*aa + dd*ba)::(dc*ab + dd*bb)::Nil
      }else{
        pow(squ(l), m/2)
      }
    }

    val List(_, _, ba, _) = pow(List(1, 1, 1, 0), n)
    ba
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
