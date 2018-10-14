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
  	
  	while(m >=1){
  		c = a
  		a = a+b
  		b = c
  		m = m - 1
  	}
  	b
  }

  //An implementation of the Fibonacci function using matrix products
  
  case class Gyouretu(a1:BigInt, a2:BigInt, a3:BigInt, a4:BigInt)
  
  def GyouretuSeki(A:Gyouretu, B:Gyouretu):Gyouretu = {Gyouretu(
  A.a1 * B.a1 + A.a2 * B.a3,
  A.a1 * B.a2 + A.a2 * B.a4,
  A.a3 * B.a1 + A.a4 * B.a3,
  A.a3 * B.a2 + A.a4 * B.a4
  )
}

  val H = Gyouretu(1, 1, 1, 0)
  
  def pow(A:Gyouretu, n:Int):Gyouretu = {
  		n match{
  			case 0 => Gyouretu(1, 0, 0, 1)
  			case _  => {
  				if(n%2 == 1) GyouretuSeki(A, pow(GyouretuSeki(A, A), (n-1)/2))
  				else pow(GyouretuSeki(A, A), n/2) 
  		}
  		}
  }
      
  def fib_matrix(n: Int): BigInt = {
  	val K:Gyouretu = pow(H, n)
  	n match{
  	  case 0 => 0
  	  case 1 => 1
  	  case _ => K.a3
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
