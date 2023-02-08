import scala.util.control.Breaks._
import java.io.Console

def gcd(a : Int, b : Int) : Int = {
    if (b == 0) {
        return a
    } else {
        return gcd(b, a % b)
    }
}

object Main extends App {
    var p: Int = System.console().readLine("Enter p : ").toInt
    var k: Int = System.console().readLine("Enter k : ").toInt
    val start: Int = 2
    val end: Int = p - 1
    val r = new scala.util.Random
    var flag: Boolean = true
    breakable {
        for (i <- 1 to k) {
            var a: Int = start + r.nextInt((end - start) + 1)
            if (gcd(a, p) > 1) {
                println("The number p isn't a prime number")
                flag = false
                break
            }
            var b: Int = a
            for (j <- 2 until (p - 1) / 2) {
                b = b * a % p
            }
            var s: Int = a / p
            if (a % p > 0) {
                s += 1
            }
            if (b == s) {
                println("The number p isn't a prime number")
                flag = false
                break
            }
        }
    }
    if (flag) {
        println("The number p is prime with probability 1 - 2^(-" + k.toString + ")")
    }
}