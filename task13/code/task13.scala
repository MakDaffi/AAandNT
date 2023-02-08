import java.io.Console
import scala.util.control.Breaks._
import scala.collection.immutable.ArraySeq

def print_polynomial(a: Array[Int]) : Unit = {
    for (i <- a.length - 1 to 1 by -1) {
        if (a(i) != 0) {
            print(a(i).toString + " * x^" + i.toString + " + ")
        }
    }
    print(a(0))
    println()
}

def div_on_field(c : Int, d : Int, n : Int) : Int = {
    var q: Int = -1
    breakable {
        for (i <- 0 until n) {
            if (i * d % n == c) {
                q = i
                break
            }
        }
    }
    return q
}

def pdf(c : Array[Int], d : Array[Int], p: Int) : Tuple2[Array[Int], Array[Int]] = {
    var r: Array[Int] = c.clone()
    var m: Int = c.length - 1
    var n: Int = d.length - 1
    var q: Array[Int] = new Array[Int](m - n + 1)
    for (i <- m - n to 0 by -1) {
        q(i) = div_on_field(r(n + i), d(n), p)
        if (q(i) == -1) {
            println("Polynomials are not divisible by each other in the given field")
            return (Array(-1), Array(-1))
        } 
        for (j <- n + i to i by -1) {
            r(j) = r(j) - q(i) * d(j - i) % p
            if (r(j) < 0) {
                r(j) = p + r(j)
            }
        }
    }
    return (q, r)
}

object Main extends App {
    var n: Int = System.console().readLine("Enter n : ").toInt
    var c: Array[Int] = System.console().readLine("Enter odds first polynomial: ").split(' ').map(_.toInt)
    var d: Array[Int] = System.console().readLine("Enter odds second polynomial: ").split(' ').map(_.toInt)
    var ans: Tuple2[Array[Int], Array[Int]] = pdf(c, d, n)
    if (!(ans._1.length == 1 & ans._1(0) == -1)) {
        println("Quotient of polynomials:")
        print_polynomial(ans._1)
        println("Remainder from dividing polynomials")
        print_polynomial(ans._2)
    }
}