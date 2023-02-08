import scala.util.control.Breaks._
import java.io.Console

def naive_method(a: Int): Vector[Int] = {
    var answers: Vector[Int] = Vector()
    var n: Int = a
    var i: Int = 2
    while (n > 1) {
        while (n % i == 0) {
            answers :+= i
            n = n / i
        }
        i += 1
    }
    return answers
}

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
        if (naive_method(p).length < 3)
            println("The number p is a prime number")
        else 
            println("The number p is the Carmichael number and isn't prime")
    } else {
        println("The number p isn't a prime number")
    }
}