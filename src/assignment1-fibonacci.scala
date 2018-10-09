package prg1.assignment1

object FIB {
  //An implementation of the Fibonacci function using the definition of the Fibonacci number
  def fib_rec(n: Int): BigInt = n match {
    case 0 | 1 => n
    case _ => fib_rec(n-1) + fib_rec(n-2)
  }

  //An implementation of the Fibonacci function using iteration (tail recursion)

  def fib_itr(n: Int): BigInt = {
    var a : BigInt = 1
    var b : BigInt = 0
    for(i <- 1 to n){
      val tmp : BigInt = a
      a += b
      b = tmp
    }
    /*
    var i = n
    while(i > 1){
      val tmp : BigInt = a
      a = b;
      b += tmp
      i -= 1
    }*/
    return b
  }



  //An implementation of the Fibonacci function using matrix products
  def fib_matrix(n: Int): BigInt = {

    /* expecting for any element of R^(2 x 2)
    def mult(A : Array[BigInt], B : Array[BigInt]): Array[BigInt] ={
      val a00 = A(0) * B(0) + A(1) * B(2)
      val a01 = A(0) * B(1) + A(1) * B(3)
      val a10 = A(2) * B(0) + A(3) * B(2)
      val a11 = A(2) * B(1) + A(3) * B(3)
      return Array(a00 , a01 , a10 ,  a11)
    }*/

    def mult_sp(A : Array[BigInt], B : Array[BigInt]): Array[BigInt] ={ // N. B. expecting A = f^p, B = f^q, but f = [[1, 1], [1, 0]]
      val tmp1 = A(0) * B(0) + A(1) * B(2)
      val tmp2 = A(0) * B(1) + A(1) * B(3)
      return Array(tmp1, tmp2, tmp2, tmp1 - tmp2)
    }

    def pow_sp(A : Array[BigInt], m : Int): Array[BigInt] = {
      if(m == 0){ // m is 0
        return Array(1, 0, 0, 1)
      }
      else{
        if(m % 2 == 0){ // m is even
          pow_sp(mult_sp(A, A), m / 2)
        }
        else{ // m is odd
          mult_sp(A, pow_sp(mult_sp(A, A) , (m - 1) / 2))
        }
      }
    }

    val A = pow_sp(Array(1, 1, 1, 0), n)
    return A(1)
  }



  //An implementation of the Fibonacci function using polynomial products
  def fib_polynomial(n: Int): BigInt = {
    def reduct(a : BigInt, b : BigInt, c : BigInt, d: BigInt, k : Int) : BigInt ={
      /* define : a, b, c, d as following rules
      * P(x) = a + bx ; liner
      * Q(x) = 1 + cx + dx^2 : quadratic
      */
      if(k == 0){
        return a
      }
      else{
        val C : BigInt = (2 * d) - (c * c)
        val D : BigInt = d * d
        if(k % 2 == 0){
          val B : BigInt = (a * d) - (b * c)
          return reduct(a, B, C, D, k / 2)
        }
        else{
          val A : BigInt = b - (a * c)
          val B : BigInt = b * d
          return reduct(A, B, C, D, k / 2)
        }
      }
    }
    return reduct(0, 1, -1, -1, n)
  }

  def bench(f: Int => BigInt, n: Int, name: String): Unit = {
    val start = System.nanoTime()
    val r = f(n)
    val end = System.nanoTime()
    println(r)
    println(name + ": " + (" " * (20 - name.length)) + (end-start) + "ns")
  }

  def main(arg: Array[String]): Unit = {
    val n = 32

    bench(fib_rec, n, "fib_rec")
    bench(fib_itr, n, "fib_itr")
    bench(fib_matrix, n, "fib_matrix")
    bench(fib_polynomial, n, "fib_polynomial")
  }
}
