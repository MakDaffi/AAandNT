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
}

def horner(p: Array[Int], n: Int): Boolean = {
    var a: Int = p(p.length - 1)
    for (i <- p.length - 2 to 0 by -1) {
        a = n * a + p(i)
    }
    return a == 0
}

def find_roots(p: Array[Int]): Unit = {
    var b: Int = p(0).abs
    var div: Array[Int] = Array()
    for (i <- 1 to b) {
        if (b % i == 0) {
            div :+= i
            div :+= -i
        }
    }
    println("Roots:")
    for(i <- 0 until div.length) {
        if(horner(p, div(i))) {
            println(div(i))
        } 
    }
}

def pow(num : Int, deg : Int) : Int = {
    var ans: Int = 1
    for (i <- 1 to deg) {
        ans = ans * num
    }
    return ans
}

def func(p: Array[Int], x: Int): Int = {
    var y: Int = 0
    for (i <- 0 until p.length) {
        y += pow(x, i) * p(i)
    }
    return y
}

object Main extends App {
    var c: Array[Int] = System.console().readLine("Enter odds polynomial: ").split(' ').map(_.toInt)
    var f: Int = System.console().readLine("Find the roots or calculate the value of a polynomial?(0,1) ").toInt
    if (f == 0) {
        find_roots(c)
    } else {
        var x: Int = System.console().readLine("Enter x: ").toInt
        println("Value of a polynomial")
        println(func(c, x))
    }
}