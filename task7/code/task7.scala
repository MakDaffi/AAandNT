import java.io.Console
import scala.util.control.Breaks._

def Miller_Rabin_algorithm (t: Int, s: Int, k: Int, p:Int): Boolean = {
    val start: Int = 2
    val end: Int = p - 2
    val r = new scala.util.Random
    for (i <- 0 until k) {
        var a: Int = start + r.nextInt((end - start) + 1)
        var x: Int = 1
        for (j <- 0 until t) {
            x = x * a % p
        }
        if (x != 1 & x != p - 1) {
            var f: Boolean = true
            breakable {
                for (j <- 0 until s - 1) {
                    x = x * x % p
                }
                if (x == 1) return false
                else {
                    if (x == p - 1) {
                        f = false
                        break
                    }
                }
            }
            if (f) return false
        }
    }
    return true
}

object Main extends App {
    var p: Int = System.console().readLine("Enter p : ").toInt
    var k: Int = System.console().readLine("Enter k : ").toInt
    var t: Int = p - 1
    var s: Int = 0
    while (t % 2 == 0) {
        t = t / 2
        s += 1
    }
    if (Miller_Rabin_algorithm(t, s, k, p)) {
        println("The number p is prime number")
    }
    else {
        println("The number p isn't prime number")
    }
}