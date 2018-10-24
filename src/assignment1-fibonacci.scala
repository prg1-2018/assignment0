package prg1.assignment1

object FIB {
//An implementation of the Fibonacci function using the definition of the Fibonacci number
def fib_rec(n: Int): BigInt = n match {
case 0 | 1 => n
case _ => fib_rec(n-1) + fib_rec(n-2)
}

//An implementation of the Fibonacci function using iteration (tail recursion)
def fib_itr(n: Int): BigInt = {
    def aux(n: Int, a: BigInt, b: BigInt): BigInt = n match {
        case 0 => b
        case _ => aux(n-1, a+b, a)
    }
    aux(n, 1, 0)
}

//An implementation of the Fibonacci function using matrix products
def fib_matrix(n: Int): BigInt = {
    def mulMatrix(x:List[BigInt],y:List[BigInt]): List[BigInt] = (x,y) match{
        case (List(a,b,c,d),List(e,f,g,h)) => List(a*e+b*g, a*f+b*h, c*e+d*g, c*f+d*h)
        }
    val A: List[BigInt] = List(1,1,1,0)
    val I: List[BigInt] = List(1,0,0,1)
    def pow(x:List[BigInt],n:Int): List[BigInt] = n match{
        case 0 => I
        case _ => n%2 match{
            case 1 => mulMatrix(x, pow(mulMatrix(x,x), (n-1)/2))
            case 0 => pow(mulMatrix(x,x), n/2)
        }
    }
    val A_n = pow(A,n)
    A_n(2)
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

