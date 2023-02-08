import java.io.Console

def gcd (a: Int, b: Int): Tuple3[Int, Int, Int] = {
    if (a == 0) {
        return (b, 0, 1)
    }
    val p = gcd (b % a, a)  
    val d = p._1
    val x1 = p._2
    val y1 = p._3
    val x = y1 - (b / a) * x1
    val y = x1
    return (d, x, y)
}

object Main extends App {
    var a: Int = System.console().readLine("Enter a : ").toInt
    var b: Int = System.console().readLine("Enter b : ").toInt
    var m: Int = System.console().readLine("Enter m : ").toInt
    a = a % m
    b = b % m
    var d: Int = gcd(a, m)._1
    if (b % d != 0) {
        println("The comparison hasn't solution")
    } else {
        println("The comparison has " + d + " solutions")
        a = a / d
        b = b / d
        m = m / d
        val ans_typle = gcd(a, m)
        var x_0 = ans_typle._2 * b % m
        if (x_0 < 0) {
            x_0 = m + x_0
        }
        print("Comparison solutions: ")
        for (i <- 0 until d) {
            print(x_0 + m * i + " ")
        }
    }
}