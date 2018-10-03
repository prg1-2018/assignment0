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
    	for{i <- 5 to 20} {
    		//println(i + "open!!")
    		fib_matrix(1<<i) should equal (fib_itr(1<<i))
    	}
    	for{i <- 5 to 25} {
    		//println(i + "open!!")
    		fib_matrix(1<<i) should equal (fib_polynomial(1<<i))
    	}   
    }
}



