import java.io.Console
import scala.util.control.Breaks._
import scala.collection.immutable.ArraySeq

def gcd(a : Int, b : Int) : Int = {
    if (b == 0) {
        return a
    } else {
        return gcd(b, a % b)
    }
}

def print_polynomial(a: Array[Int]) : Unit = {
    for (i <- a.length - 1 to 1 by -1) {
        if (a(i) != 0) {
            print(a(i).toString + " * x^" + i.toString + " + ")
        }
    }
    print(a(0))
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

def cont(a: Array[Int]): Int = {
    var c: Int = a(0)
    for (i <- 1 until a.length) {
        c = gcd(c, a(i))
    }
    return c
}

def div(a: Array[Int], n: Int): Array[Int] = {
    var b: Array[Int] = a.clone()
    for (i <- 0 until a.length) {
        b(i) /= n
    }
    return b
}

def mult(a: Array[Int], n: Int): Array[Int] = {
    var b: Array[Int] = a.clone()
    for (i <- 0 until a.length) {
        b(i) *= n
    }
    return b
}

def pdf(c : Array[Int], d : Array[Int]) : Tuple2[Array[Int], Array[Int]] = {
    var r: Array[Int] = c.clone()
    var m: Int = c.length - 1
    var n: Int = d.length - 1
    var q: Array[Int] = new Array[Int](m - n + 1)
    for (i <- m - n to 0 by -1) {
        q(i) = r(n + i) / d(n)
        for (j <- n + i to i by -1) {
            r(j) = r(j) - q(i) * d(j - i)
        }
    }
    return (q, r)
}

def preob(a_x: Array[Int]): Array[Int] = {
    var l: Int = a_x.length - 1
    breakable {
        while(l > 0) {
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

def pow(num : Int, deg : Int) : Int = {
    var ans: Int = 1
    for (i <- 1 to deg) {
        ans = ans * num
    }
    return ans
}

def geap(a_x: Array[Int], b_x: Array[Int]): Array[Int] = {
    var c: Int = gcd(cont(a_x), cont(b_x))
    var p1: Array[Int] = div(a_x, cont(a_x))
    var p2: Array[Int] = div(b_x, cont(b_x))
    breakable {
        while(true) {
            var l = pow(p2(p2.length - 1), p1.length - p2.length + 1)
            p1 = mult(p1, l)
            var r: Array[Int] = pdf(p1, p2)._2
            r = preob(r)
            if (r.length != 1) {
                r = div(r, cont(r))
            }
            if (r.length == 1 & r(0) == 0) {
                break
            }
            p1 = p2
            p2 = r
        }
    }
    var f = false
    if (p2.length == 1) {
        return Array(c)
    }
    return mult(p2, c)
}

def derivative(p1: Array[Int]): Array[Int] = {
    var p2: Array[Int] = new Array[Int](p1.length - 1)
    for (i <- 0 until p2.length) {
        p2(i) = p1(i + 1) * (i + 1)
    }
    return p2
}

def psqff(p: Array[Int]): Array[Array[Int]] = {
    var r: Array[Int] = geap(p, derivative(p))
    var t: Array[Int] = pdf(p, r)._1
    if (r.length == 1) {
        return Array(t)
    }
    var s: Array[Array[Int]] = Array()
    breakable {
        while (true) {
            var v: Array[Int] = geap(r, t)
            s :+= preob(pdf(t, v)._1)
            r = preob(pdf(r, v)._1)
            t = v
            if (r.length == 1) {
                s :+= t
                break
            }
        }
    }
    return s
}

object Main extends App {
    var c: Array[Int] = System.console().readLine("Enter odds polynomial: ").split(' ').map(_.toInt)
    var ans: Array[Array[Int]] = psqff(c)
    println("Square-free polynomial factors:")
    for (i <- 0 until ans.length) {
        if (ans(i).length != 1) {
            print("(")
            print_polynomial(ans(i))
            print(")^" + (i+1).toString)
            println()
        }
    }
}