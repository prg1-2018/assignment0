package prg1.assignment1

object FIB {
  //An implementation of the Fibonacci function using the definition of the Fibonacci number
  def fib_rec(n: Int): BigInt = n match {
    case 0 | 1 => n
    case _ => fib_rec(n-1) + fib_rec(n-2)
  }

  //An implementation of the Fibonacci function using iteration (tail recursion)
  def fib_itr(n: Int): BigInt = {
    def fib_tmp(m:Int,a:BigInt,b:BigInt):BigInt = m match{
    	case 0  => b
    	case _ => fib_tmp(m-1,a+b,a)
    }
    fib_tmp(n,1,0)
  }

  //An implementation of the Fibonacci function using matrix products
  def fib_matrix(n: Int): BigInt = {
    def matrix_mul(a:(BigInt,BigInt,BigInt,BigInt),b:(BigInt,BigInt,BigInt,BigInt)):(BigInt,BigInt,BigInt,BigInt)=a match{
    	case (a11,a12,a21,a22) => b match{
    		case (b11,b12,b21,b22) => 
    		(a11*b11+a12*b21,a11*b12+a12*b22,a21*b11+a22*b21,a21*b12+a22*b22)
    	}
    }
    def matrix_bekijo(a:(BigInt,BigInt,BigInt,BigInt),m:Int):(BigInt,BigInt,BigInt,BigInt) = {
    	if(m==0)(1,0,0,1)
    	else if(m%2==1)matrix_mul(a,matrix_bekijo(a,m-1))
    	else matrix_bekijo(matrix_mul(a,a),m/2)
    }
    matrix_bekijo( (1,1,1,0),n ) match{
    	case (x11,x12,x21,x22) => x21
    }
  }

  //An implementation of the Fibonacci function using polynomial products
  def fib_polynomial(n: Int): BigInt = {
    def poly_unko(m:Int,p:(BigInt,BigInt),q:(BigInt,BigInt,BigInt)):BigInt = p match{
    	case (p1,p0) => q match{
    		case (q2,q1,q0) => {
    			if(m==0) p0
    			else if(m%2==0)poly_unko( m/2, (p0*q2-p1*q1,p0*q0), ( q2*q2,2*q2*q0-q1*q1,q0*q0 ) )
    			else poly_unko( (m-1)/2, (p1*q2,p1*q0-p0*q1), ( q2*q2,2*q2*q0-q1*q1,q0*q0 ) )
    		}
    	}
  	}
  	poly_unko(n,(1,0),(-1,-1,1))
  }

  def bench(f: Int => BigInt, n: Int, name: String): Unit = {
    val start = System.nanoTime()
    val r = f(n)
    val end = System.nanoTime()
    println(r)
    println(name + ": " + (" " * (20 - name.length)) + (end-start) + "ns")
  }

  def main(arg: Array[String]): Unit = {
    val n = 20

    bench(fib_rec, n, "fib_rec")
    bench(fib_itr, n, "fib_itr")
    bench(fib_matrix, n, "fib_matrix")
    bench(fib_polynomial, n, "fib_polynomial")
  }
}
