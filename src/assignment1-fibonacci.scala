//package prg1.assignment1
//
object FIB {
  //An implementation of the Fibonacci function using the definition of the Fibonacci number
  def fib_rec(n: Int): BigInt = n match {
    case 0 | 1 => n
    case _ => fib_rec(n-1) + fib_rec(n-2)
  }

  //An implementation of the Fibonacci function using iteration (tail recursion)
  def fib_itr(n: Int): BigInt = {
    n match {
      case 0 => 0
      case 1 => 1
      case _ =>{
        var f0: BigInt = 0;
        var f1: BigInt = 1;
        var f2: BigInt = 1;
        for (i <- 2 to n){
          f2 = f0 + f1;
          f0 = f1;
          f1 = f2;
        }
        f2;
      }
    }
  }

  //An implementation of the Fibonacci function using matrix products
  def fib_matrix(n: Int): BigInt = {
    def isEven(n : Int): Boolean = {
      (n % 2) == 0
    }
    n match{
      case 0 => 0
      case 1 => 1
      case _ =>{
//        var ma = Array[BigInt](1, 1, 1, 0);
        var mama: Array[BigInt] = null;
        def mm(a: Array[BigInt], b: Array[BigInt]): Array[BigInt] = {
          var c: Array[BigInt] = null;
          c(0) = a(1)*b(1) + a(2)*b(3);
          c(1) = a(1)*b(2) + a(2)*b(4);
          c(2) = a(3)*b(1) + a(4)*b(3);
          c(3) = a(3)*b(2) + a(4)*b(4);
          c;
        }
        mama = mm(ma, ma);
        isEven(n) match{
          case true => {
            for(i <- 1 to n/4){
              mama = mm(mama, mama);
            }
          }
          case false =>{
            for(i <- 1 to (n-1)/4){
              mama = mm(mama, mama);
            }
            mama = mm(ma, mama);
          }
        }
        mama(3);
      }
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
    //bench(fib_matrix, n, "fib_matrix")
    //bench(fib_polynomial, n, "fib_polynomial")
  }
}
