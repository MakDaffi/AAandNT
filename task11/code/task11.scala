import java.io.Console
import scala.util.control.Breaks._
import scala.math.BigInt
import scala.collection.mutable.Stack
import scala.util.Random

def sqrt(n: BigInt) : BigInt = {
  val d = BigDecimal(n)
  var a = BigDecimal(1.0)
  var b = d
  while(b - a >= 0) {
    val mid = (a + b) / 2
    if (mid * mid - d > 0) {
        b = mid - 0.0001
    }
    else {             
        a = mid + 0.0001
    }
  }
  return b.toBigInt
}

def fermat_method(a: BigInt): Tuple2[BigInt, BigInt] = {
    var x: BigInt = sqrt(a)
    var y: BigInt = 0
    breakable {
        while (true) {
            y = x * x - a
            var b: BigInt = sqrt(y)
            if (b * b == y | (b + 1) * (b + 1) == y) {
                break
            }
            x += 1
        }
    }
    var b: BigInt = sqrt(y)
    if ((b + 1) * (b + 1) == y) {
        b = b + 1
    }
    return (x + b, x - b)
}

object Main extends App {
    var a: BigInt = BigInt(System.console().readLine("Enter your number for factorization: "))
    var answers: Vector[BigInt] = Vector()
    val n: BigInt = a
    while (a % 2 == 0) {
        a = a / 2
        answers :+= 2
    }
    var s = Stack[BigInt]()
    if (a != 1) {
        s.push(a)
    }
    while (!(s.isEmpty)) {
        val cur: BigInt = s.pop
        val p = fermat_method(cur)
        if (p._2 == 1) {
            answers :+= p._1 
        }
        else {
            s.push(p._1)
            s.push(p._2)
        }
    }
    if (answers.length > 1) {
        print(n.toString + " = ")
        for (i <- 0 until answers.length - 1) {
            print(answers(i).toString + " * ")
        }
        println(answers(answers.length - 1))
    } else println("The number " + n.toString + " is prime number")
}