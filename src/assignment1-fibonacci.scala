package prg1.assignment1

object FIB {
  //An implementation of the Fibonacci function using the definition of the Fibonacci number
  def fib_rec(n: Int): BigInt = n match {
    case 0 | 1 => n
    case _ => fib_rec(n-1) + fib_rec(n-2)
  }

  //An implementation of the Fibonacci function using iteration (tail recursion)
  def fib_itr(n: Int): BigInt = {
    
    def sub_itr(a: Int, b: Int, m: Int): BigInt = {
        if(m==0){b}
        else{sub_itr(a+b,a,m-1)}}
    
    sub_itr(1,0,n)
  }

  //An implementation of the Fibonacci function using matrix products
  def fib_matrix(n: Int): BigInt = {
    
    case class mat2(a: BigInt,b: BigInt,c: BigInt,d: BigInt)
    
    def mul(A: mat2, B: mat2): mat2 = (A,B) match {
        case (mat2(b11,b12,b21,b22),mat2(c11,c12,c21,c22)) =>
            mat2(b11*c11+b12*c21, b11*c12+b12*c22, b21*c11+b22*c21, b21*c12+b22*c22)}
    
    def pow(B: mat2, n: Int): mat2 = {
        if (n==0) {mat2(1,0,0,1)}
        else if (n%2==0) {pow(mul(B,B),n/2)}
        else {mul(B,pow(mul(B,B),(n-1)/2))}}
    
    val A = mat2(1,1,1,0)
    
    pow(A,n) match {
        case mat2(f11,f12,f21,f22) => f21
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
