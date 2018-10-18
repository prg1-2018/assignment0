package prg1.assignment1

import org.scalatest._
import FIB._

class FIBTest extends FunSuite with Matchers {
   test("FIB tests") {
     fib_rec(0) should equal (fib_itr(0))
     
     fib_rec(5) should equal (fib_itr(5))
     
     fib_rec(6) should equal (fib_itr(6))
     
     fib_itr(0) should equal (fib_matrix(0))
     
     fib_itr(1) should equal (fib_matrix(1))
     
     fib_itr(5) should equal (fib_matrix(5))
     
     fib_itr(25) should equal (fib_matrix(25))
     
     fib_itr(90) should equal (fib_matrix(90))
     
     
     
     
     
   }

}



