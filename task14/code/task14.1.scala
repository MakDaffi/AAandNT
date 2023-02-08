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

def mult(a_x: Array[Int], b_x: Array[Int], n: Int): Array[Int] = {
    var a_x1: Array[Int] =  new Array[Int](a_x.length + b_x.length - 1)
    for (i <- 0 until a_x.length) {
        for (j <- 0 until b_x.length) {
            a_x1(i + j) = (a_x1(i + j) + a_x(i) * b_x(j) % n) % n
        }
    }
    return a_x1
}

def preob(a_x: Array[Int]): Array[Int] = {
    var l: Int = a_x.length - 1
    breakable {
        while(l >= 0) {
            if (a_x(l) != 0) {
                break
            }
            l -= 1
        }
    }
    var a: Array[Int] = Array()
    for (i <- 0 to l) {
        a :+= a_x(i) 
    }
    return a
}

def minus(a_x: Array[Int], b_x: Array[Int], n: Int): Array[Int] = {
    var len: Int = a_x.length.max(b_x.length)
    var ans: Array[Int] = new Array[Int](len)
    for (i <- 0 until a_x.length) {
        ans(i) = a_x(i)
    }
    for (i <- 0 until len) {
        ans(i) -= b_x(i)
        if (ans(i) < 0) {
            ans(i) = n + ans(i)
        }
    }
    return ans
}

def isn(p : Array[Int]): Boolean = {
    var f: Boolean = true
    for (i <- p) {
        if (i != 0) {
            f = false
        }
    }
    return f
}

def gcd(a_x: Array[Int], b_x: Array[Int], n: Int): Tuple3[Array[Int], Array[Int], Array[Int]] = {
    var p1: Array[Int] = a_x
    var p2: Array[Int] = b_x
    var p0: Array[Int] = p1
    p1 = p2
    var g0: Array[Int] = Array(1)
    var g1: Array[Int] = Array(0)
    var f0: Array[Int] = Array(0)
    var f1: Array[Int] = Array(1)
    var q: Array[Int] = Array()
    while(!isn(p1)) {
        q = preob(pdf(p0, p1, n)._1)
        var b: Array[Int] = preob(minus(p0, mult(p1, q, n), n))
        p0 = p1
        p1 = b
        b = preob(minus(g0, mult(g1, q, n), n))
        g0 = g1
        g1 = b
        b = preob(minus(f0, mult(f1, q, n), n))
        f0 = f1
        f1 = b
    }
    return (p0, g0, f0)
}

object Main extends App {
    var n: Int = System.console().readLine("Enter n : ").toInt
    var c: Array[Int] = System.console().readLine("Enter odds first polynomial: ").split(' ').map(_.toInt)
    var d: Array[Int] = System.console().readLine("Enter odds second polynomial: ").split(' ').map(_.toInt)
    var ans: Tuple3[Array[Int], Array[Int], Array[Int]] = gcd(c, d, n)
    print("GCD : ")
    print_polynomial(ans._1)
    print("First odd : ")
    print_polynomial(ans._2)
    print("Second odd : ")
    print_polynomial(ans._3)
}