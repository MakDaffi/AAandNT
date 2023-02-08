import java.io.Console
import scala.util.control.Breaks._
import java.lang.Math.{log, sqrt, exp}
import scala.util.Random
import scala.collection.immutable.ArraySeq

def print_array(a: Array[Int]): Unit = {
    for (i <- 0 until a.length) {
        print(a(i).toString + " ")
    }
    println()
}

def gcd(a : Int, b : Int) : Int = {
    if (b == 0) {
        return a
    } else {
        return gcd(b, a % b)
    }
}

def pow(num : Int, deg : Int) : Int = {
    var ans: Int = 1
    var i: Int = 1
    while (i <= deg) {
        ans = ans * num
        i += 1
    }
    return ans
}

def prime_check(num: Int): Boolean = {
    val end: Double = sqrt(num.toDouble)
    for (i <- 2 to end.toInt) {
        if (num % i == 0) {
            return false
        }
    }
    return true
}

def gen_all_comb(len: Int): Array[Array[Int]] = {
    var comb: Array[Array[Int]] = Array()
    var c: Array[Int] = Array()
    for (i <- 0 until len) {
        c :+= -1
    }
    comb :+= c.clone()
    for (i <- 0 until pow(3, len) - 1) {
        var j: Int = 0
        while(c(j) == 1) {
            c(j) = -1
            j += 1
        }
        c(j) += 1
        comb +:= c.clone()
    }
    return comb
}

def null_check(a: Array[Int]): Boolean = {
    var flag: Boolean = true
    for (i <- a) {
        if (i != 0) {
            flag = false
        }
    }
    return flag
}

def find_ld(comb: Array[Array[Int]], target: Array[Array[Int]]) : Array[Int] = {
    for (c <- comb) {
        var ans: Array[Int] = Array()
        for (i <- 0 until c.length) {
            ans +:= 0
        }
        for (i <- 0 until target.length) {
            for (j <- 0 until target(i).length) {
                ans(j) = ans(j) + c(i) * target(i)(j)
            }
        }
        if (null_check(ans) & !null_check(c)) {
            return c
        }
    }
    return Array(-1)
}

def dixon_method(n: Int): Unit = {
    val m: Int = sqrt(exp(sqrt(log(n.toDouble) * log(log(n.toDouble))))).toInt
    var B: Array[Int] = Array()
    for (i <- 2 to m) {
        if (prime_check(i)) {
            B :+= i
        }
    }
    val start: Int = sqrt(n.toDouble).toInt
    val end: Int = n
    val rnd = new Random
    var alpha: Array[Array[Int]] = Array()
    var eps: Array[Array[Int]] = Array() 
    var b_arr: Array[Int] = Array()
    while(b_arr.length != B.length + 1) {
        var b: Int = start + rnd.nextInt( (end - start) + 1 )
        var a: Int = b * b % n
        //println(b)
        var cur_vec: Array[Int] = Array()
        var cur_eps: Array[Int] = Array()
        for (i <- 0 until B.length) {
            var cur_elem: Int = 0
            while(a % B(i) == 0) {
                cur_elem += 1
                a /= B(i) 
            }
            cur_vec :+= cur_elem
            cur_eps :+= cur_elem % 2
        }
        if (a == 1) {
            alpha :+= cur_vec
            eps :+= cur_eps
            b_arr :+= b
        }
    }
    val comb: Array[Array[Int]] = gen_all_comb(eps.length)
    val ld: Array[Int] = find_ld(comb, eps)
    var x: Int = 1
    var y: Int = 1
    var deg_arr: Array[Int] = new Array[Int](B.length)
    for (i <- 0 until ld.length) {
        if (ld(i) != 0 & !null_check(eps(i))) {
            x = (x * b_arr(i)) % n
            for (j <- 0 until B.length) {
                deg_arr(j) += alpha(i)(j)
            }
        }
    }
    for (i <- 0 until deg_arr.length) {
        y = y * pow(B(i), deg_arr(i) / 2) % n
    }
    if (x != y & x != n - y) {
        println(gcd(x + y, n))
        println(gcd(x - y, n))
    }
}

object Main extends App {
    var a: Int = System.console().readLine("Enter your number for factorization: ").toInt
    var answers: Vector[Int] = Vector()
    println("The factors of the number " + a.toString + " are:")
    dixon_method(a)
}