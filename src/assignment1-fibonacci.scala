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
    var a : BigInt = 1
    var b : BigInt = 0
    var c : BigInt = 0
    while(m >= 1){
      c = b
      b = a
      a = b + c
      m -= 1
    }
    b
  }

  //An implementation of the Fibonacci function using matrix products
  def fib_matrix(n: Int): BigInt = {
      def gyouretuseki(A: List[BigInt],B: List[BigInt]): List[BigInt] = {
        (A,B) match{
          case(List(a,b,c,d),List(e,f,g,h))=> List(a*e+b*g,a*f+b*h,c*e+d*g,c*f+d*h)
        }
      }
      def kaijyoukeisan(C: List[BigInt],m: Int): List[BigInt] ={
        if(m == 0){List(1,0,0,1)}
        else if(m%2 == 0){kaijyoukeisan(gyouretuseki(C,C),m/2)}
        else {gyouretuseki(C,kaijyoukeisan(gyouretuseki(C,C),(m-1)/2))}
        }
        kaijyoukeisan(List(1,1,1,0),n)
        match {
          case(List(a,b,c,d)) => c
        }
      }




  //An implementation of the Fibonacci function using polynomial products
  def fib_polynomial(n: Int): BigInt = {
    def takousikiseki(A: (BigInt, BigInt,BigInt), B: (BigInt, BigInt,BigInt),m: Int): (BigInt, BigInt,BigInt) = {
      val (a0,a1,a2) = A
      val (b0,b1,b2) = B
      if(m % 2 == 0){(a0*b0,-a1*b1+a0*b2+a2*b0,a2*b2)}
      else{(-a0*b1+a1*b0,-a2*b1+a1*b2,0)}
    }
    def keisukeisan(P: (BigInt, BigInt,BigInt), Q: (BigInt, BigInt,BigInt),l: Int): BigInt = {
      val (p0,p1,p2) = P
      if(l == 0){p0}
      else if(l%2 == 0){keisukeisan(takousikiseki(P,Q,l),takousikiseki(Q,Q,0),l/2)}
      else{keisukeisan(takousikiseki(P,Q,l),takousikiseki(Q,Q,0),(l-1)/2)}
    }
    keisukeisan((0,1,0),(1,-1,-1),n)
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
