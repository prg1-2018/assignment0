package prg1.assignment1

object FIB {
  //An implementation of the Fibonacci function using the definition of the Fibonacci number
  def fib_rec(n: Int): BigInt = n match {
    case 0 | 1 => n
    case _ => fib_rec(n-1) + fib_rec(n-2)
  }

  //An implementation of the Fibonacci function using iteration (tail recursion)
  def fib_itr(n: Int): BigInt = n match{
    case 0 | 1 => n
    case _ =>{
    var a: BigInt = 1
    var b : BigInt = 0
    var c : BigInt = 0
    var m = n
  while ( m >= 1){
   c=a
   a=a+b
   b=c
    m=m-1
    }
    {b}
  }
}
  //An implementation of the Fibonacci function using matrix products
  case class Matrix(a11: BigInt, a12: BigInt, a21: BigInt, a22:BigInt)
 def mult(a:Matrix,b:Matrix):Matrix =(a,b) match
	{case(Matrix(a11: BigInt, a12: BigInt, a21: BigInt, a22:BigInt),Matrix(b11: BigInt, b12: BigInt, b21: BigInt, b22:BigInt))
	=>
	{Matrix(a11*b11+a12*b21,a11*b12+a12*b22,a21*b11+a22*b21,a21*b12+a22*b22)}
	}
	
 def pow:
	(Matrix,Int)=>Matrix
	=
	(A,n)=>
	{n match {
	case 0 =>Matrix(1,0,0,1)
	case _ =>
	{ if(n%2==0){pow(mult(A,A),n/2)}
	  else mult(A,pow(mult(A,A),(n-1)/2))
	}
	}
	}

 /*def get :
		(Matrix) => BigInt
		=
		(A)=>
		A match {case Matrix(a,b,c,d)=>{c}} 
*/		

  def	fib_matrix(n:Int):BigInt = n match{
 	case 0 => 0
 	case _ => pow(Matrix(1,1,1,0),n).a21 
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