package prg1.assignment1

object FIB {
  //An implementation of the Fibonacci function using the definition of the Fibonacci number
  def fib_rec(n: Int): BigInt = n match {
    case 0 | 1 => n
    case _ => fib_rec(n-1) + fib_rec(n-2)
  }

  //An implementation of the Fibonacci function using iteration (tail recursion)
  def fib_itr(n: Int): BigInt = {
  	def calc(a:BigInt, b:BigInt ,m:Int): BigInt = m match {
  	case 0  => b
  	case _ => calc(a+b, a, m-1)
  	}
  	calc(1,0,n)
  }

  //An implementation of the Fibonacci function using matrix products
  def fib_matrix(n: Int): BigInt = {
	// 2*2と2*2 or 2*1の行列積をもとめる
  	def  arrprd(lst1: List[BigInt] ,lst2: List[BigInt]): List[BigInt] = (lst1,lst2) match {
  		case (a00::a01::a10::a11::Nil,b00::b01::b10::b11::Nil) =>
  		List(a00*b00+a01*b10, a00*b01+a01*b11, a10*b00+a11*b10, a10*b01+a11*b11)
  		case (a00::a01::a10::a11::Nil,b00::b10::Nil) =>
  		List(a00*b00+a01*b10, a10*b00+a11*b10)
  		case _ => Nil
  	}
  	def pow(A: List[BigInt],m: Int): List[BigInt] = m match {
  		case 0 => List(1,0,0,1)
  		case _ => m%2 match {
  			case 1 => arrprd(A, pow(arrprd(A, A) , (m-1)/2))
  			case _ => pow(arrprd(A, A), m/2)
  		}
  	}
  	arrprd(pow(List(1,1,1,0), n), List(1,0)) match {
  		case a::b::Nil => b
  		case _ => -1
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
