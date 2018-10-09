package prg1.assignment1

import org.scalatest._
import FIB._

class FIBTest extends FunSuite with Matchers {
   test("FIB tests") {
     fib_itr(2) should equal (fib_rec(2))
     fib_itr(5) should equal (fib_rec(5))
     fib_itr(10) should equal (fib_rec(10))
     fib_itr(20) should equal (fib_rec(20))
     fib_itr(30) should equal (fib_rec(30))
     fib_itr(30) should equal (fib_matrix(30))
     fib_itr(50) should equal (fib_matrix(50))
     fib_itr(75) should equal (fib_matrix(75))
     fib_itr(100) should equal (fib_matrix(100))
     fib_itr(200) should equal (fib_matrix(200))
     fib_itr(500) should equal (fib_matrix(500))
     fib_itr(1000) should equal (fib_matrix(1000))
     fib_itr(10000) should equal (fib_matrix(10000))
  }

}
