package prg1.assignment1

import org.scalatest._
import FIB._

class FIBTest extends FunSuite with Matchers {
   test("FIB tests") {
     fib_itr(0) should equal (fib_rec(0))
     fib_matrix(0) should equal (fib_itr(0))
     fib_polynomial(0) should equal (fib_matrix(0))

     for(i <- 1 to 10){
       fib_itr(i) should equal (fib_rec(i))
     }
     for(i <- 1 to 20){
       fib_matrix(i) should equal (fib_itr(i))
     }
     for(i <- 1 to 50){
       fib_polynomial(i) should equal (fib_matrix(i))
     }
     fib_polynomial(100) should equal (fib_matrix(100))
     fib_polynomial(1000) should equal (fib_matrix(1000))
   }

}
