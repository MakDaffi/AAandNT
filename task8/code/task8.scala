import java.io.Console
import java.lang.Math.{log10, sqrt}
import scala.math.BigInt
import scala.util.control.Breaks._
import scala.collection.immutable.ArraySeq

def phi (n : Int) : Int = {
    var n1: Int = n
    var result: Int = n
    var i: Int = 2
    while (i * i <= n1) {
        if (n1 % i == 0) {
            while (n1 % i == 0)
                n1 = n1 / i;
            result -= result / i;
        }
        i += 1
    }
    if (n1 > 1)
        result -= result / n1;
    return result
}

def pow(num : Int, deg : Int, mod : Int) : Int = {
    var ans: Int = 1
    for (i <- 1 to deg) {
        ans = ans * num % mod
    }
    return ans
}

def o_r(n: Int, r: Int): Int = {
    if (r == n) return -1
    var a: Int = n % r
    var ans: Int = 1
    while (a != 1) {
        a = a * n % r
        ans += 1
    }
    return ans
}

def is_degree(n: Int): Boolean = {
    val end: Int = sqrt(n).toInt
    for (i <- 2 to end) {
        var a: Int = i
        while (a < n) {
            a *= i
            if (a == n) return true
        }
    }
    return false
}

def mult(a_x: Array[BigInt], b_x: Array[BigInt]): Array[BigInt] = {
    var a_x1: Array[BigInt] =  Array[BigInt]()
    for (j <- 0 to a_x.length + b_x.length - 2) {
        a_x1 :+= 0
    }
    for (i <- 0 until a_x.length) {
        for (j <- 0 until b_x.length) {
            a_x1(i + j) += a_x(i) * b_x(j)
        }
    }
    return a_x1
}

def agrawala_kayala_saxene_test(n: Int): Boolean = {
    if (is_degree(n)) return false
    val log2 = (x: Double) => log10(x)/log10(2.0)
    var form: Double = log2(n.toDouble)
    form *= form
    var r = 2
    breakable {
        while(true) {
            if (n % r == 0 & r != n) {
                return false
            } else {
                if (o_r(n, r).toDouble > form) {
                    break
                } else {
                    r += 1
                }
            }
        }
    }
    if (n <= r) return true
    val end: Int = (sqrt(phi(r)) * log2(n)).toInt
    var flag: Boolean = true
    for (i <- 1 to end) {
        var a_x: Array[BigInt] =  Array[BigInt](i, 1)
        val a_x1: Array[BigInt] = a_x
        while(a_x.length != n + 1) {
            a_x = mult(a_x, a_x1)
        }
        a_x(0) -= i
        a_x(n) -= 1
        for (j <- 0 to n) {
            if(a_x(j) % n != 0) {
                flag = false
            }
        }
    }
    return flag
}

object Main extends App {
    var n: Int = System.console().readLine("Enter n : ").toInt
    if (agrawala_kayala_saxene_test(n)) {
        println("The number p is prime number")
    } else {
        println("The number p isn't prime number")
    }
}