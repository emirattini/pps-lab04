package u04lab

import u03.Sequences.*
import u03.Sequences.Sequence
import u03.Sequences.Sequence.*

import java.util.function.BiFunction
import scala.annotation.tailrec

/*  Exercise 4: 
 *  - Complete the implementation of ad-hoc polymorphic sumAll, using summable.sum and summable.zero
 *  - Write givens also for Summable[Double], Summable[String]
 *  - Uncomment in the main and check if everything works
 */

object Ex4Summables:

  def sumAllInt(seq: Sequence[Int]): Int = seq match
    case Cons(h, t) => h + sumAllInt(t)
    case _ => 0

  trait Summable[A]:
    def sum(a1: A, a2: A): A
    def zero: A

  @tailrec
  def foldLeft[A, B](s: Sequence[A])(start: B)(op: BiFunction[B, A, B]): B = s match
      case Cons(h, t) => foldLeft(t)(op(start, h))(op)
      case _ => start

  def sumAll[A: Summable](seq: Sequence[A]): A =
    val summable = summon[Summable[A]]
    foldLeft(seq)(summable.zero)(summable.sum)

  given Summable[Int] with
    def sum(a1: Int, a2: Int): Int = a1 + a2
    def zero: Int = 0
  given Summable[Double] with
    def sum(a1: Double, a2: Double): Double = a1 + a2
    def zero: Double = 0.0
  given Summable[String] with
    def sum(s1: String, s2: String): String = s1 + s2
    def zero: String = "0"

  // write givens for Summable[Double] and Summable[String]

  @main def trySummables =
    val si = Cons(10, Cons(20, Cons(30, Nil())))  
    println:
      sumAllInt(si) // 60

    println:
      sumAll(si) // 60

    val sd = Cons(10.0, Cons(20.0, Cons(30.0, Nil())))  
    println:
      sumAll(sd) // 60.0

    val ss = Cons("10", Cons("20", Cons("30", Nil())))  
    println:
      sumAll(ss) // "102030"



