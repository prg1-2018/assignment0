package prg1.assignment1

object FIB {
  //An implementation of the Fibonacci function using the definition of the Fibonacci number
  def fib_rec(n: Int): BigInt = n match {
    case 0 | 1 => n
    case _ => fib_rec(n-1) + fib_rec(n-2)
  }

  //An implementation of the Fibonacci function using iteration (tail recursion)

  def fib_itr(n: Int): BigInt = {
    var m : BigInt =n
    var a : BigInt =1
    var b : BigInt =0
    while (m >= 1){
    var c: BigInt =a
    a=a+b
    b=c
    m=m-1 }
    return b	
     }

  //An implementation of the Fibonacci function using matrix products
case class matrix(a1:BigInt,a2:BigInt,a3:BigInt,a4:BigInt)

def dot (a:matrix,b:matrix):matrix={
	a match {case matrix(a1,a2,a3,a4) => b match{case matrix(b1,b2,b3,b4)=>
	                  matrix(a1*b1+a2*b3,a1*b2+a2*b4,a3*b1+a4*b3,a3*b2+a4*b4) }}}

def double (a:matrix):matrix={
	a match {case matrix(a1,a2,a3,a4) => matrix(a1*a1+a2*a3,a1*a2+a2*a4,a1*a3+a3*a4,a2*a3+a4*a4)}}

def A=matrix(1,1,1,0)

def pow(a:matrix,n:Int):matrix={
	if (n==0){  return matrix(1,0,0,1) }
	else if (n%2==1) { return dot(a,pow(double(a),(n-1)/2)) }
	else {return pow(double(a),n/2) }}

def fib(n:Int):BigInt={
	pow(A,n) match {case matrix(a1,a2,a3,a4) => a3}}

  def fib_matrix(n: Int): BigInt = {
    pow(A,n) match {case matrix(a1,a2,a3,a4) => a3}
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
