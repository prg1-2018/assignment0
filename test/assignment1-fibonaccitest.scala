package prg1.assignment1

import org.scalatest._
import FIB._

class FIBTest extends FunSuite with Matchers {
   test("FIB tests") {
     fib_itr(0) should equal (fib_rec(0))
     fib_itr(0) should equal (fib_matrix(0))
     fib_matrix(0) should equal (fib_polynomial(0))
     fib_itr(1) should equal (fib_rec(1))
     fib_itr(1) should equal (fib_matrix(1))
     fib_matrix(1) should equal (fib_polynomial(1))
     fib_itr(2) should equal (fib_rec(2))
     fib_itr(5) should equal (fib_rec(5))
     fib_itr(10) should equal (fib_rec(10))
     fib_itr(20) should equal (fib_rec(20))
     fib_itr(23) should equal (fib_rec(23))
     fib_itr(30) should equal (fib_rec(30))
     fib_itr(30) should equal (fib_matrix(30))
     fib_itr(47) should equal (fib_matrix(47))
     fib_itr(50) should equal (fib_matrix(50))
     fib_itr(61) should equal (fib_matrix(61))
     fib_itr(75) should equal (fib_matrix(75))
     fib_itr(88) should equal (fib_matrix(88))
     fib_itr(100) should equal (fib_matrix(100))
     fib_itr(133) should equal (fib_matrix(133))
     fib_itr(152) should equal (fib_matrix(152))
     fib_itr(177) should equal (fib_matrix(177))
     fib_itr(200) should equal (fib_matrix(200))
     fib_itr(295) should equal (fib_matrix(295))
     fib_itr(346) should equal (fib_matrix(346))
     fib_itr(500) should equal (fib_matrix(500))
     fib_itr(1000) should equal (fib_matrix(1000))
     fib_itr(10000) should equal (fib_matrix(10000))
     fib_matrix(100000) should equal (fib_polynomial(100000))
     fib_matrix(100001) should equal (fib_polynomial(100001))
     fib_matrix(1000000) should equal (fib_polynomial(1000000))
     fib_matrix(1000001) should equal (fib_polynomial(1000001))
     fib_matrix(5000000) should equal (fib_polynomial(5000000))
  }

}
