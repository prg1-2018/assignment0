package prg1.assignment1

import org.scalatest._
import FIB._

class FIBTest extends FunSuite with Matchers {
   test("S_FIB tests") {
    	for{i <- 0 to 20} fib_itr(i) should equal (fib_rec(i))
    	for{i <- 0 to 20} {
    		//println(i + "open!!")
    		fib_matrix(i) should equal (fib_rec(i))
    	}
    	for{i <- 0 to 20} {
    		//println(i + "open!!")
    		fib_polynomial(i) should equal (fib_rec(i))
    	}   
    }
    test("L_FIB tests") {
        var k = 9
    	for{i <- 0 to 10} {
    	    k = k*3
    		//println(i + "open!!")
    		fib_matrix(k) should equal (fib_itr(k))
    		fib_matrix(k) should equal (fib_polynomial(k))
    	}
    }
}



