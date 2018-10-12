package prg1.assignment1

object FIB {
  //An implementation of the Fibonacci function using the definition of the Fibonacci number
  def fib_rec(n: Int): BigInt = n match {
    case 0 | 1 => n
    case _ => fib_rec(n-1) + fib_rec(n-2)
  }

  //An implementation of the Fibonacci function using iteration (tail recursion)
  def fib_itr(n: Int): BigInt = {
  	//0

    var m:BigInt = n
    var a:BigInt = 1
    var b:BigInt = 0

    var v:BigInt = 0
    while(m >= 1){
    	v = a
    	a += b
    	b = v
    	m -= 1
    }
    b

  }

  //An implementation of the Fibonacci function using matrix products
  def fib_matrix(n: Int): BigInt = {
    pow(List(1,1,1,0),n)(2)
  }

  def pow(A:List[BigInt],n: Int): List[BigInt] = {
    val A2 :List[BigInt] = mlt(A,A)
    if(n==0){
      List(1,0,0,1)
    }else if(n%2 == 1){
      mlt(A,pow(A2,n/2))
    }else{
      pow(A2,n/2)
    }
  }

  def mlt(P:List[BigInt],Q:List[BigInt]): List[BigInt] = {
    val a0 : BigInt = P(0)*Q(0)+P(1)*Q(2)
    val a1 : BigInt = P(0)*Q(1)+P(1)*Q(3)
    val a2 : BigInt = P(2)*Q(0)+P(3)*Q(2)
    val a3 : BigInt = P(2)*Q(1)+P(3)*Q(3)
    List(a0,a1,a2,a3)
  }

  //An implementation of the Fibonacci function using polynomial products
  def fib_polynomial(n: Int): BigInt = {
    pol(n,0,1,-1,-1)
  }

  def pol(n: Int, p0: BigInt, p1: BigInt, q1: BigInt, q2: BigInt): BigInt = {
    val nq1 : BigInt = 2*q2-q1*q1
    val nq2 : BigInt = q2*q2
    if(n==0){
      p0
    }else if(n%2==0){
      pol(n/2, p0, p0*q2-p1*q1, nq1, nq2)
    }else{
      pol(n/2, p1-p0*q1, p1*q2, nq1, nq2)
    }
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
