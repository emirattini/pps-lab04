package u04.drawmynumber

import u04.monads.States.State

import scala.util.Random

enum Result:
  case OutOfBound, Win, Loss, More, Less

trait DrawMyNumberState:
  type DrawMyNumber
  def initialState(): DrawMyNumber
  def initialState(topBound: Int, attempts: Int): DrawMyNumber
  def getTopBoundAndAttempts: State[DrawMyNumber, (Int, Int)]
  def nop(): State[DrawMyNumber, Unit]
  def guess(x: Int): State[DrawMyNumber, (Result, Int)]

object DrawMyNumberStateImpl extends DrawMyNumberState:
  import Result.*
  opaque type DrawMyNumber = (Int, Int, (Int, Int))

  def initialState(): DrawMyNumber = (new Random().nextInt(100), 10, (100, 10))
  def initialState(topBound: Int, attempts: Int): DrawMyNumber =
    (new Random().nextInt(topBound), attempts, (topBound, attempts))

  def getTopBoundAndAttempts: State[DrawMyNumber, (Int, Int)] = State(i => (i, i._3))
  
  def nop(): State[DrawMyNumber, Unit] = State(i => (i, ()))

  def guess(x: Int): State[DrawMyNumber, (Result, Int)] =
    State((num, attemptsLeft, startingModel) => (num, startingModel) match
      case (num, (topBound, _)) if x > topBound || x < 0 => ((num, attemptsLeft, startingModel), (OutOfBound, topBound))
      case (num, (topBound, maxAttempts)) if num == x => (initialState(topBound, maxAttempts), (Win, attemptsLeft - 1))
      case (num, (topBound, maxAttempts)) if attemptsLeft - 1 == 0 => (initialState(topBound, maxAttempts), (Loss, num))
      case (num, _) if num < x => ((num, attemptsLeft - 1, startingModel), (Less, attemptsLeft - 1))
      case _ => ((num, attemptsLeft - 1, startingModel), (More, attemptsLeft - 1)))
