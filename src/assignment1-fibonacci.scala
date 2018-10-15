package prg1.assignment1


object FIB {
  //An implementation of the Fibonacci function using the definition of the Fibonacci number
  def fib_rec(n: Int): BigInt = n match {
    case 0 | 1 => n
    case _ => fib_rec(n-1) + fib_rec(n-2)
  }

  //An implementation of the Fibonacci function using iteration (tail recursion)
  def fib_itr(n: Int): BigInt = {
    var m=n
    var a:BigInt=1
    var b:BigInt=0
    def hozyo(m:Int,a:BigInt,b:BigInt): BigInt ={
    	if(m==0){b}
    	else{hozyo(m-1,a+b,a)}
    }
    hozyo(m,a,b)
  }

  //An implementation of the Fibonacci function using matrix products
  def fib_matrix(n: Int): BigInt = {
    
    def gyoretu(a:List[BigInt],n:Int): List[BigInt]={
		a match{
			case(List(a1,a2,a3,a4))=>{
		if(n==0){List(0,0,0,0)}
    	else if(n==1){List(a1,a2,a3,a4)}
    	else if(n%2==1){kakezan(List(a1,a2,a3,a4),gyoretu(List(a1*a1+a2*a3,a1*a2+a2*a4,a1*a3+a4*a3,a2*a3+a4*a4),(n-1)/2))}
    	else{gyoretu(List(a1*a1+a2*a3,a1*a2+a2*a4,a1*a3+a4*a3,a2*a3+a4*a4),n/2)}
    	}}
    	
    	
    	
    }
    
    def kakezan(a:List[BigInt],b:List[BigInt]):List[BigInt]={
			(a,b) match{
				case(List(a1,a2,a3,a4),List(b1,b2,b3,b4))=>{
	List(a1*b1+a2*b3,a1*b2+a2*b4,a3*b1+a4*b3,a3*b2+a4*b4)
	}}}
	
    def toridasu(a:List[BigInt]):BigInt={
		a match{
			case(List(a1,a2,a3,a4))=>a3
			}
		}
    toridasu(gyoretu(List(1,1,1,0),n))
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
    val n = 0

    bench(fib_rec, n, "fib_rec")
    bench(fib_itr, n, "fib_itr")
    bench(fib_matrix, n, "fib_matrix")
    bench(fib_polynomial, n, "fib_polynomial")
  }
}
