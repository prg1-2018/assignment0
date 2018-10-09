package prg1.assignment1

import org.scalatest._
import FIB._

class FIBTest extends FunSuite with Matchers {
   test("FIB tests - small") {
     fib_itr(2) should equal (fib_rec(2))

     for(i <- 2 to 30){
       val rec = fib_rec(i)
       val itr = fib_itr(i)
       val mat = fib_matrix(i)
       val poly = fib_polynomial(i)
       rec should equal (itr)
       itr should equal (mat)
       mat should equal (poly)
     }
   }
   test("FIB tests - medium") {
     for(i <- 30 to 1000){
       val itr = fib_itr(i)
       val mat = fib_matrix(i)
       val poly = fib_polynomial(i)
       itr should equal (mat)
       mat should equal (poly)
     }
   }
   test("FIB tests - large") {
     for(i <- 1000 to 10000){
       fib_matrix(i) should equal (fib_polynomial(i))
     }
   }
   test("FIB tests - Larger") {
     for(i <- 10007 to 100000 by 10297){
       fib_matrix(i) should equal (fib_polynomial(i))
     }
     for(i <- 100003 to 1000000 by 316742){
       fib_matrix(i) should equal (fib_polynomial(i))
     }

     val check = 1000003
     fib_matrix(check) should equal (fib_polynomial(check))
     val check2 = (1 << 20) + 1
     fib_matrix(check) should equal (fib_polynomial(check))
   }

}
