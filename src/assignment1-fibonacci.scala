package prg1.assignment1

object FIB {
  //An implementation of the Fibonacci function using the definition of the Fibonacci number
  def fib_rec(n: Int): BigInt = n match {
    case 0 | 1 => n
    case _ => fib_rec(n-1) + fib_rec(n-2)
  }

  //An implementation of the Fibonacci function using iteration (tail recursion)
  def fib_itr(n: Int): BigInt = {
     var a:BigInt = 1
     var b:BigInt = 0
    for {m <- 1 to n}{
    var c = a
    a = a + b
    b = c
  }
  b
  }

  //An implementation of the Fibonacci function using matrix products
  def fib_matrix(n: Int): BigInt = {
    def complex(A:List[BigInt],B:List[BigInt]): List[BigInt] = {
      (A,B) match{
        case (List(a1,a2,a3,a4),List(b1,b2,b3,b4)) =>//行列の計算
        List(a1*b1+a2*b3,a1*b2+a2*b4,a3*b1+a4*b3,a3*b2+a4*b4)
      }

    }

    def pow(A:List[BigInt],n:Int): List[BigInt] = {
      if (n == 0) List(1,0,0,1)
      else if (n%2 == 1) complex(A,pow(complex(A,A),(n-1)/2))
      else pow(complex(A,A),n/2)
    }
    pow(List(1,1,1,0),n) match{
      case List(a,b,c,d) => c
    }
  }

  //An implementation of the Fibonacci function using polynomial products
  def fib_polynomial(n: Int): BigInt = {
    def  fib(n:Int,p:List[BigInt],q:List[BigInt]): BigInt ={

        def pxq1(A:List[BigInt],B:List[BigInt]): List[BigInt] ={
          //p×q(-x)を計算しxの指数が偶数のものだけを残す(nが偶数の時のp'(x))
          (A,B) match{
              case (List(a1,a2),List(b1,b2,b3)) =>
              //List(a1*b1,a2*b1-b2*a1,a1*b3-a2*b2,a2*b3)
              List(a1*b1,a1*b3-a2*b2)
            }
            }
        def  pxq2(A:List[BigInt],B:List[BigInt]): List[BigInt] ={
          //p×q(-x)を計算しxの指数が奇数のものだけを残す(nが奇数の時のp'(x))
          (A,B) match{
              case (List(a1,a2),List(b1,b2,b3)) =>
              //List(a1*b1,a2*b1-b2*a1,a1*b3-a2*b2,a2*b3)
              List(a2*b1-b2*a1,a2*b3)
            }
            }

        def qxq(A:List[BigInt],B:List[BigInt]): List[BigInt] ={
          //q(x)×q(-x)を計算し、xの指数が偶数のものだけを残す(q'(x)を計算)
          (A,B) match{
              case (List(a1,a2,a3),List(b1,b2,b3)) =>
              //List(a1*b1,a2*b1-a1*b2,a1*b3-a2*b2+a3*b1,a2*b3-a3*b2,a3*b3)
              List(a1*b1,a1*b3-a2*b2+a3*b1,a3*b3)
                }
            }

    if(n == 0){
        p match{
            case Nil => 0
            case e::rest => e
        }
    }
    else if(n%2 == 0){
        fib(n/2,pxq1(p,q),qxq(q,q))
    }
    else{
         fib((n-1)/2,pxq2(p,q),qxq(q,q))

     }

    }
      fib(n,List(0,1),List(1,-1,-1))
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
