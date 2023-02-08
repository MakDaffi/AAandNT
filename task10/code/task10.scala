import java.io.Console
import java.lang.Math.{log10, sqrt}
import scala.math.BigInt
import scala.util.control.Breaks._
import scala.util.Random

def gcd(a : BigInt, b : BigInt) : BigInt = {
    if (b == 0) {
        return a
    } else {
        return gcd(b, a % b)
    }
}

def prime_check(num: Int): Boolean = {
    val end: Double = sqrt(num.toDouble)
    for (i <- 2 to end.toInt) {
        if (num % i == 0) {
            return false
        }
    }
    return true
}

def pow(num : BigInt, deg : BigInt) : BigInt = {
    var ans: BigInt = 1
    var i: BigInt = 1
    while (i <= deg) {
        ans = ans * num
        i += 1
    }
    return ans
}

def poklington_test(p1: Int, p2: Int, max_a: Int): Boolean = {
    var q: Int = p1.max(p2)
    var n: BigInt = p1 * p2 + 1
    for (i <- 2 to max_a) {
        if ((pow(i, n - 1) % n == 1) & (gcd(pow(i, (n - 1) / q) - 1, n) == 1)) {
            return true
        }
    }
    return false
}

object Main extends App {
    var max_a: Int = System.console().readLine("Enter maximal a for check : ").toInt
    var flag: Boolean = false
    var p1: Int = 0
    var p2: Int = 0
    val start: Int = 2
    val end: Int  = 99
    val rnd = new Random
    while(!flag) {
        p1 = start + rnd.nextInt( (end - start) + 1 )
        p2 = start + rnd.nextInt( (end - start) + 1 )
        if (prime_check(p1.max(p2))) {
            flag = poklington_test(p1, p2, max_a)
        }
    }
    var n: BigInt = p1 * p2 + 1
    println(n)
}