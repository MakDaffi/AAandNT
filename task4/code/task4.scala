import scala.util.control.Breaks._
import java.io.Console

object Main extends App {
    var p: Int = System.console().readLine("Enter p : ").toInt
    var c: Int = 1
    breakable {
        for (i <- 2 to p) {
            if (p % i != 0) {
                c = i 
                break
            }
        }
    }
    var b: Int = c
    for (i <- 2 until p) {
        b = b * c % p
    }
    if (b == 1) {
        println("The number p is a prime number")
    } else {
        println("The number p isn't a prime number")
    }
}