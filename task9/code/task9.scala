import java.io.Console
import java.lang.Math.{log10, sqrt}
import scala.math.BigInt
import scala.util.control.Breaks._
import scala.util.Random

def pow(num : BigInt, deg : Int) : BigInt = {
    var ans: BigInt = 1
    for (i <- 1 to deg) {
        ans = ans * num
    }
    return ans
}

def luc_lehmer_test(p: Int): Boolean = {
    val m: BigInt = pow(2, p) - 1
    var s: BigInt = 4
    var k: Int = 1
    while(k != p - 1) {
        s = (s * s - 2) % m
        k += 1
    }
    if (s == 0) {
        return true
    } else {
        return false
    }
}

object Main extends App {
    var flag: Boolean = false
    var p: Int = 0
    val start = 3
    val end  = 9999
    val rnd = new Random
    while(!flag) {
        p = start + rnd.nextInt( (end - start) + 1 )
        if (p % 2 == 1) {
            flag = luc_lehmer_test(p)
        }
    }
    println("The result was a number 2^" + p.toString + " - 1 = " + (pow(2, p) - 1).toString)
}