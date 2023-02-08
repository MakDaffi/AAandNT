import java.io.Console

object Main extends App {
    var p: Int = System.console().readLine("Enter p : ").toInt
    var b: Int = 1
    for (i <- 2 until p) {
        b = b * i % p
    }
    if (b == p - 1) {
        println("The number p is a prime number")
    } else {
        println("The number p isn't a prime number")
    }
}