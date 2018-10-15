package prg1.assignment1

import org.scalatest._
import FIB._

class FIBTest extends FunSuite with Matchers {
   test("FIB test1") {

     for {m <- 0 to 20}{
     fib_itr(m) should equal (fib_rec(m))
     fib_matrix(m) should equal (fib_rec(m))
     fib_polynomial(m) should equal (fib_rec(m))
   }

   }

   test("FIB test2"){
     for {m <- 21 to 100}{
     fib_matrix(m) should equal (fib_itr(m))
     fib_polynomial(m) should equal (fib_itr(m))
   }
   }

   test("FIB test3"){
     for {m <- 101 to 10000}{
     fib_polynomial(m) should equal (fib_matrix(m))
   }
   }

}
