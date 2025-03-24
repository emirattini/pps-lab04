package u04lab
import u03.Sequences.*
import Sequence.*
import u04.monads.Optionals.Optional
import Optional.*
import u04lab.Ex5Traversable.{log, logAll}

import java.util.function.Consumer

/*  Exercise 5: 
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others... 
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a datastructure T[A] 
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

object Ex5Traversable:
  
  trait Traversable[T[_]]:
    def traverse[A](t: T[A])(f: A => Unit): Unit

  given Traversable[Sequence] with
    def traverse[A](s: Sequence[A])(f: A => Unit): Unit = s match
      case Cons(h, t) => f(h); traverse(t)(f)
      case _ => ()

  given Traversable[Optional] with
    def traverse[A](opt: Optional[A])(f: A => Unit): Unit = opt match
      case Just(a) => f(a)
      case _ => ()

  def log[A](a: A): Unit = println("The next element is: "+a)

  def logAll[T[_]: Traversable, A](t: T[A])(f: A => Unit): Unit =
    val traversable = summon[Traversable[T]]
    traversable.traverse(t)(f)

@main def tryTraversables =
  val seq1 = Cons(10, Cons(20, Cons(30, Nil())))
  val seq2 = Cons("Hi", Cons("How", Cons("Are", Cons("You?", Nil()))))
  val empty = Empty()
  val opt = Just("Hello world!")

  logAll(seq1)(log)
  logAll(seq2)(log)
  logAll(empty)(log)
  logAll(opt)(log)
  
  logAll(seq1)(println)
  logAll(seq2)(a => println(a))
  logAll(empty)(a => println(a))
  logAll(opt)(a => println(a))



  
