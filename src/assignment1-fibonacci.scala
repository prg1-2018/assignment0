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
    var c:BigInt = 0
    while(m >= 1){
      c = b
      b = a
      a = a + c
      m = m - 1
    }
    return b
  }

  //An implementation of the Fibonacci function using matrix products
  def fib_matrix(n: Int): BigInt = {

    def pow(A: List[BigInt],n : Int) : List[BigInt] = {
      if(n == 0) return List(1,0,0,1)
      else if (n % 2 == 1) {
      A match {case a1::a2::a3::a4::Nil =>
       pow(List(a1*a1+a2*a3,a1*a2+a2*a4,a1*a3+a3*a4,a2*a3+a4*a4),(n - 1) / 2) match {
        case b1::b2::b3::b4::Nil =>
        return List(a1*b1+a2*b3,a1*b2+a2*b4,a3*b1+a4*b3,a3*b2+a4*b4)
       }
      }
    }
      else  {
      A match {case a1::a2::a3::a4::Nil =>
      return pow(List(a1*a1+a2*a3,a1*a2+a2*a4,a1*a3+a3*a4,a2*a3+a4*a4),n / 2)
      }
    }
  }
   pow(List(1,1,1,0),n) match{case a::b::c::d::Nil => return c
   }
}
  //An implementation of the Fibonacci function using polynomial products
  def fib_polynomial(n: Int): BigInt = {
    def polycalculation(A:List[BigInt],B:List[BigInt],p: Int) : List[BigInt] = {
      if(p % 2 == 0) {
        (A,B) match {case (a1::a2::a3::Nil,b1::b2::b3::Nil) =>
        return List(a1*b1,-a2*b2+a1*b3+a3*b1,a3*b3)}
      }
      else {(A,B) match {case (a1::a2::a3::Nil,b1::b2::b3::Nil) =>
        return List(-a1*b2+a2*b1,-a3*b2+a2*b3,0)}
      }
    }
      def coefcalculation(C:List[BigInt],D:List[BigInt],q: Int):BigInt ={
        if(q == 0){C match {case c1::c2::c3::Nil =>
        return c1}}
        else if(q % 2 == 0){
          return coefcalculation(polycalculation(C,D,q),polycalculation(D,D,0),q/2)}
        else return   coefcalculation(polycalculation(C,D,q),polycalculation(D,D,0),(q-1)/2)
      }
    coefcalculation(List(0,1,0),List(1,-1,-1),n)
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
