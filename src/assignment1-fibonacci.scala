package prg1.assignment1

object FIB {
  //An implementation of the Fibonacci function using the definition of the Fibonacci number
  def fib_rec(n: Int): BigInt = n match {
    case 0 | 1 => n
    case _ => fib_rec(n-1) + fib_rec(n-2)
  }

  //An implementation of the Fibonacci function using iteration (tail recursion)
  def fib_itr(n: Int): BigInt = {
    def fib_itr_sub(a:BigInt, b:BigInt, m:Int): BigInt = (a,b,m) match {
        case (_,_,0) => b
        case (_,_,_) => fib_itr_sub(a+b,a,m-1)
    }
    fib_itr_sub(1,0,n)
  }

  //An implementation of the Fibonacci function using matrix products
  def fib_matrix(n: Int): BigInt = {
    def multMatrix(lst1:List[BigInt], lst2:List[BigInt]):List[BigInt] = {
      (lst1,lst2) match{
        case (List(a1,b1,c1,d1),List(a2,b2,c2,d2)) => List(a1*a2+b1*c2,a1*b2+b1*d2,c1*a2+d1*c2,c1*b2+d1*d2)
        case (_,_) => Nil
      }
    }
    def pow(lst:List[BigInt], n:Int): List[BigInt] = {
      if (n == 0){List(1,0,0,1)}
      else if(n%2 == 1){multMatrix(lst,pow(multMatrix(lst,lst),(n-1)/2))}
      else {pow(multMatrix(lst,lst),n/2)}
    }
    pow(List(1,1,1,0),n) match{
      case List(a,b,c,d) => c
      case _ => 0
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
