package prg1.assignment1

object FIB {
  //An implementation of the Fibonacci function using the definition of the Fibonacci number
  def fib_rec(n: Int): BigInt = n match {
    case 0 | 1 => n
    case _ => fib_rec(n-1) + fib_rec(n-2)
  }

  //An implementation of the Fibonacci function using iteration (tail recursion)
  def fib_itr(n: Int): BigInt = {
  	//変数定義
    var a:BigInt = 1
    var b:BigInt = 0
    var c:BigInt = 0
    var m:BigInt = n
    
    // n ≥ 1として、n-1, n番目のフィボナッチ数をn, n+1番目に更新する
    while(m >= 1) {
    	c = a
    	a = a + b
    	b = c
    	m = m - 1
    }
    
    //出力
    b
  }
  
  //２×２行列の型の定義
  case class Matrix(a11: BigInt, a12: BigInt, a21: BigInt, a22: BigInt)

  //二つの行列の積を求める。
  def mult(a: Matrix, b: Matrix): Matrix = {
	val Matrix(x, y, z, w) = a
	val Matrix(k, l, m, n) = b
	Matrix(x*k+y*m, z*k+w*m, x*l+y*n, z*l+w*n)
  }
  
  //入力された行列aのn乗を求める。
  def pow(a: Matrix, n: Int): Matrix = n match {
  	case 0 => Matrix(1, 0, 0, 1)
  	case _ => if (n%2 == 0) pow(mult(a, a), n/2)
  					 else mult(a, pow(mult(a, a), (n-1)/2))
  }
 
  //An implementation of the Fibonacci function using matrix products
  def fib_matrix(n: Int): BigInt = {
  	// A=Matrix(1, 1, 1, 0)として、A^nを求める。
  	val a = pow(Matrix(1, 1, 1, 0), n)
  	
  	//出力
  	a.a21
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
