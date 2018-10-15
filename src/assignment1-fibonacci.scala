package prg1.assignment1

object FIB {
    //An implementation of the Fibonacci function using the definition of the Fibonacci number
    def fib_rec(n: Int): BigInt = n match {
        case 0 | 1 => n
        case _ => fib_rec(n-1) + fib_rec(n-2)
    }
    
    //An implementation of the Fibonacci function using iteration (tail recursion)
    def fib_itr(n: Int): BigInt = {
        def fib_itr_rec(a: BigInt, b: BigInt, m: Int): BigInt = m match {
            case 0 => b
            case _ => fib_itr_rec(a+b, a, m-1)
        }
        fib_itr_rec(1, 0, n)
    }

    //An implementation of the Fibonacci function using matrix products
    def fib_matrix(n: Int): BigInt = {

    	case class Mat2(e11: BigInt, e12: BigInt, e21: BigInt, e22: BigInt)
    	
    	// 行列 a, b の積を計算する
    	def mulMat2(a: Mat2, b: Mat2) = (a, b) match {
	        case (Mat2(a11, a12, a21, a22), Mat2(b11, b12, b21, b22)) => {
	            Mat2(a11*b11+a12*b21, a11*b12+a12*b22, a21*b11+a22*b21, a21*b12+a22*b22)
	        }
	    }
	    
	    // 行列 mat を n 乗する
	    def powMat2(mat: Mat2, n: Int): Mat2 = n match {
	        case 0 => Mat2(1, 0, 0, 1)
	        case _ => {
	            if(n % 2 == 0) powMat2(mulMat2(mat, mat), n/2)
	            else mulMat2(mat, powMat2(mulMat2(mat, mat), (n-1)/2))
	        }
	    }

        n match {
        	case 0 => 0
        	case _ => powMat2(Mat2(1, 1, 1, 0), n-1).e11
        }
    }
    
    //An implementation of the Fibonacci function using polynomial products
    def fib_polynomial(n: Int): BigInt = {
        getCoefficientOfFps(Poly(0, 1, 0), Poly(1, -1, -1), n)
    }
    case class Poly(a0: BigInt, a1: BigInt, a2: BigInt)
    def getCoefficientOfFps(p: Poly, q: Poly, n: Int): BigInt = n match {
        
		case 0 => p.a0
		case _ => {
			val Poly(p0, p1, p2) = p
			val Poly(q0, q1, q2) = q
			if(n % 2 == 0) getCoefficientOfFps(
				Poly(p0*q0, p0*q2-p1*q1+p2*q0, p2*q2),
				Poly(q0*q0, 2*q0*q2-q1*q1, q2*q2),
				n / 2
			)
			else getCoefficientOfFps(
				Poly(-p0*q1+p1*q0, p1*q2-p2*q1, 0),
				Poly(q0*q0, 2*q0*q2-q1*q1, q2*q2),
				(n - 1) / 2
			)
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
        val n1 = 30
        print("n = 30\n")
        bench(fib_rec, n1, "fib_rec")
        bench(fib_itr, n1, "fib_itr")
        bench(fib_matrix, n1, "fib_matrix")
        bench(fib_polynomial, n1, "fib_polynomial")

        val n2 = 100
        print("n = 100\n")
        bench(fib_itr, n2, "fib_itr")
        bench(fib_matrix, n2, "fib_matrix")
        bench(fib_polynomial, n2, "fib_polynomial")
    }
}
