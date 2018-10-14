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
    var a :BigInt = 1
    var b :BigInt = 0
    var temp :BigInt = 0
    while(m>=1){
      temp = b
      b = a
      a = a + temp
      m = m-1
    }
    b
  }

  //An implementation of the Fibonacci function using matrix products
case class Matrix(a: BigInt,b: BigInt,c: BigInt,d: BigInt)
  def fib_matrix(n: Int): BigInt = {

    def pow(matrix: Matrix,n: Int): Matrix = {

      def products(matrix1: Matrix ,matrix2: Matrix) : Matrix = {
        val Matrix(a1,b1,c1,d1) = matrix1
        val Matrix(a2,b2,c2,d2) = matrix2
        Matrix(a1*a2+b1*c2,a1*b2+b1*d2,c1*a2+d1*c2,c1*b2+d1*d2)
      }

      if(n==0) Matrix(1,0,0,1)
      else if(n%2!=0) products(matrix,pow(products(matrix,matrix),(n-1)/2))
      else pow(products(matrix,matrix),n/2)
    }

    val Matrix(a,b,c,d) = pow(Matrix(1,1,1,0),n)
    c
  }

  //An implementation of the Fibonacci function using polynomial products
case class Polynomial(a: BigInt,b: BigInt,c: BigInt)
  def fib_polynomial(n: Int): BigInt = {

    def multPoly(poly1: Polynomial,poly2: Polynomial,eventrue: Boolean): Polynomial = {
      val Polynomial(a0,a1,a2) = poly1
      val Polynomial(b0,b1,b2) = poly2

      if(eventrue) Polynomial(a0*b0,a2*b0-a1*b1+a0*b2,a2*b2)
      else Polynomial(a1*b0-a0*b1,-a2*b1+a1*b2,0)
    }

    def fib(p: Polynomial,q: Polynomial,n: Int): BigInt = {
      val evenq : Polynomial = multPoly(q,q,true)
      val Polynomial(p0,p1,p2) = p

      if(n==0) p0
      else if(n%2==0) fib(multPoly(p,q,true),evenq,n/2)
      else fib(multPoly(p,q,false),evenq,(n-1)/2)
    }

    fib(Polynomial(0,1,0),Polynomial(1,-1,-1),n)
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
