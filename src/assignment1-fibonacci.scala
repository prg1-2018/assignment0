package prg1.assignment1

object FIB {
  //An implementation of the Fibonacci function using the definition of the Fibonacci number
  def fib_rec(n: Int): BigInt = n match {
    case 0 | 1 => n
    case _ => fib_rec(n-1) + fib_rec(n-2)
  }

  //An implementation of the Fibonacci function using iteration (tail recursion)
  def fib_itr(n: Int): BigInt = {
    def Ntimespl(n: Int,a: BigInt,b: BigInt): BigInt = n match{
    	case 0 => b
    	case _ => Ntimespl(n-1,a+b,a)
    }
    Ntimespl(n,1,0)
  }

  //An implementation of the Fibonacci function using matrix products
  def fib_matrix(n: Int): BigInt = {
  	/*        a          b   
  	A=[     b          c           ] として行列の積を計算をする
  	*/
  	def pow(n:Int,b:BigInt,c:BigInt): (BigInt,BigInt)= n match{
  		case 0 => (0,1)
  		case m if m%2==1 => pow((n-1)/2,b,c) match{
  			case (y,z) => ((y+z)*(y+z)+y*y,y*(y+2*z))
  		}
  		case _ => pow(n/2,b,c) match{
  			case (y,z) => (y*(y+2*z),y*y+z*z)
  		}
  	}
  	pow(n,1,0) match{
  		case (b,c) => b
  	}
  }

  //An implementation of the Fibonacci function using polynomial products
  def fib_polynomial(n: Int): BigInt = {
  	/*
  	初期のPが１次式、Qが２次式ならばP'o,P'e,Q'はそれぞれ１、１、２次以下の式となること、Qの０次の係数が帰納的に常に１となることを利用し、
  	P=ax+b
  	Q=cx^2+dx+1   とした。
  	*/
    def pol(n:Int,a:BigInt,b:BigInt,d:BigInt): (BigInt,BigInt,BigInt)= n match{
    	case 0 => (a,b,d)
    	case m if m%2==1 => pol(n/2,a,a-b*d,2-d*d)
    	case _ => pol(n/2,b-a*d,b,2-d*d)
    }
    n match{
    	case 0 => 0
    	case m if m%2==1 => pol(n/2,-1,1,-3) match{
    		case (a,b,d) => b
    	}
    	case _ => pol(n/2,1,0,-3) match{
    		case (a,b,d) => b
    	}
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

