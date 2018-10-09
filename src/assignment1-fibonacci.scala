//package prg1.assignment1
//
object FIB {
  //An implementation of the Fibonacci function using the definition of the Fibonacci number
  def fib_rec(n: Int): BigInt = n match {
    case 0 | 1 => n
    case _ => fib_rec(n-1) + fib_rec(n-2)
  }

  //An implementation of the Fibonacci function using iteration (tail recursion)
  def fib_itr(n: Int): BigInt = {
    n match {
      case 0 => 0
      case 1 => 1
      case _ =>{
        var f0: BigInt = 0;
        var f1: BigInt = 1;
        var f2: BigInt = 1;
        for (i <- 2 to n){
          f2 = f0 + f1;
          f0 = f1;
          f1 = f2;
        }
        f2;
      }
    }
  }

  //An implementation of the Fibonacci function using matrix products
  def fib_matrix(n: Int): BigInt = {
    def isEven(n : Int): Boolean = {
      (n % 2) == 0
    }
    var m: Int = n;
    n match{
      case 0 => 0
      case _ => {
        var ma = new Array[BigInt](4);
        def mm(a: Array[BigInt], b: Array[BigInt]): Array[BigInt] = {
          var c = new Array[BigInt](4);
          c(0) = a(0)*b(0) + a(1)*b(2);
          c(1) = a(0)*b(1) + a(1)*b(3);
          c(2) = a(2)*b(0) + a(3)*b(2);
          c(3) = a(2)*b(1) + a(3)*b(3);
          c;
        }
        def pow(a: Array[BigInt], m: Int): Array[BigInt] = {
          m match {
            case 0 =>{
              var c = new Array[BigInt](4);
              c(0) = 1;
              c(1) = 0;
              c(2) = 0;
              c(3) = 1;
              c;
            }
            case _ =>{
              isEven(m) match{
                case true =>{
                  pow(mm(a,a), m/2)
                }
                case false =>{
                  mm(a, pow(mm(a,a),(m-1)/2))
                }
              }
            }
          }
        }
        ma(0) = 1;
        ma(1) = 1;
        ma(2) = 1;
        ma(3) = 0;
        ma = pow(ma, n);
        ma(2);
      }
    }
  }

  //An implementation of the Fibonacci function using polynomial products
  def fib_polynomial(n: Int): BigInt = {
    def porec(m: Int, p: Array[BigInt], dp: Int, q: Array[BigInt], dq: Int): BigInt = {
      //m次の係数を返す p:P(x), q:Q(x), dp:整式Pの係数を格納する配列の末尾のラベル dq:整式Qの係数を格納する配列の末尾のラベル
      m match{
        case 0 => p(0)
        case _ =>{
          def isEven(n : Int): Boolean = {
            (n % 2) == 0
          }
          def pocon(a: Array[BigInt], da: Int, b: Array[BigInt], db: Int): Array[BigInt] = {
            //整式の積
            var c = new Array[BigInt](da+db+1);
            for(i <- 0 to da+db){
              c(i) = 0
            }
            for(i <- 0 to da){
              for(j <- 0 to db){
                c(i+j) = c(i+j) + a(i) * b(j)
              }
            }
            c
          }
          def poev(r: Array[BigInt], dr: Int): Array[BigInt] = {
            //整式のうち、偶数次数の係数のみ取り出す
            var e = new Array[BigInt](dr/2+1);
            for(i <- 0 to dr/2){
              e(i) = r(2*i);
            }
            e
          }
          def podd(r: Array[BigInt], dr: Int): Array[BigInt] = {
            //整式のうち、奇数次数の係数のみ取り出す
            isEven(dr) match{
              case true => {
                var o = new Array[BigInt](dr/2)
                for(i <- 0 to dr/2-1){
                  o(i) = r(2*i + 1);
                }
                o
              }
              case false => {
                var o = new Array[BigInt](dr/2+1);
                for(i <- 0 to dr/2){
                  o(i) = r(2*i + 1);
                }
                o
              }
            }
          }
          def revQ(qx: Array[BigInt], dqx: Int): Array[BigInt] = {
            //Q(x)の係数からQ(-x)の係数を計算する
            isEven(dqx) match{
              case true => {
                for(i <- 0 to dqx/2-1){
                  qx(2*i + 1) = -1*qx(2*i + 1);
                }
                qx
              }
              case false => {
                for(i <- 0 to dqx/2){
                  qx(2*i + 1) = -1*qx(2*i + 1);
                }
                qx
              }
            }
          }
          isEven(m) match {
            case true =>{
              porec(m/2, poev(pocon(p, dp, revQ(q, dq), dq), dp+dq), (dp+dq)/2, poev(pocon(q, dq, revQ(q, dq),  dq), dq+dq), dq);
            }
            case false =>{
              isEven(dp + dq) match{
                case true =>{
                  porec((m-1)/2, podd(pocon(p, dp, revQ(q, dq), dq), dp+dq), (dp+dq)/2-1, poev(pocon(q, dq, revQ(q, dq),  dq), dq+dq), dq);
                }
                case false =>{
                  porec((m-1)/2, podd(pocon(p, dp, revQ(q, dq), dq), dp+dq), (dp+dq)/2, poev(pocon(q, dq, revQ(q, dq),  dq), dq+dq), dq);
                }
              }
            }
          }
        }
      }
    }
    n match{
      case 0 => 0
      case _ => {
        var p0 = new Array[BigInt](2);
        var q0 = new Array[BigInt](3);
        p0(0) = 0;
        p0(1) = 1;
        q0(0) = 1;
        q0(1) = -1;
        q0(2) = -1;
        porec(n, p0, 1, q0, 2)
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
    //bench(fib_matrix, n, "fib_matrix")
    //bench(fib_polynomial, n, "fib_polynomial")
  }
}
