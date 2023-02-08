import java.io.Console

def gcd(a : Int, b : Int) : Int = {
    if (b == 0) {
        return a
    } else {
        return gcd(b, a % b)
    }
}

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

object Main extends App {
    var a: Int = System.console().readLine("Enter a : ").toInt
    var b: Int = System.console().readLine("Enter b : ").toInt
    var m: Int = System.console().readLine("Enter m : ").toInt
    a = a % m
    b = b % m
    var d: Int = gcd(a, m)
    if (b % d != 0) {
        println("The comparison hasn't solution")
    } else {
        println("The comparison has " + d + " solutions")
        a = a / d
        b = b / d
        m = m / d
        var x_0: Int = pow(a, phi(m) - 1, m) * b % m
        print("Comparison solutions: ")
        for (i <- 0 until d) {
            print(x_0 + m * i + " ")
        }
    }
}