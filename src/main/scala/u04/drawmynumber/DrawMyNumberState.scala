package u04.drawmynumber

import u04.monads.States.State

import scala.util.Random

enum Result:
  case OutOfBound, Win, Loss, More, Less

trait DrawMyNumberState:
  type DrawMyNumber
  type Attempts = Int
  def initialState(): DrawMyNumber
  def initialState(topBound: Int, attempts: Int): DrawMyNumber
  def getTopBoundAndMaxAttempts: State[DrawMyNumber, (Int, Attempts)]
  def nop(): State[DrawMyNumber, Unit] = State(i => (i, ()))
  def guess(x: Int): State[DrawMyNumber, (Result, Attempts)]

object DrawMyNumberStateImpl extends DrawMyNumberState:
  import Result.*
  opaque type DrawMyNumber = (Int, Attempts, (Int, Int))

  def initialState(): DrawMyNumber = (new Random().nextInt(100), 10, (100, 10))
  def initialState(topBound: Int, attempts: Int): DrawMyNumber =
    (new Random().nextInt(topBound), attempts, (topBound, attempts))

  def getTopBoundAndMaxAttempts: State[DrawMyNumber, (Int, Attempts)] = State(i => (i, i._3))

  def guess(x: Int): State[DrawMyNumber, (Result, Attempts)] =
    State((num, attempts, startingModel) => (num, startingModel) match
      case (num, (topBound, _)) if x > topBound || x < 0 => ((num, attempts, startingModel), (OutOfBound, topBound))
      case (num, (topBound, maxAttempts)) if num == x => (initialState(topBound, maxAttempts), (Win, attempts - 1))
      case (num, (topBound, maxAttempts)) if attempts - 1 == 0 => (initialState(topBound, maxAttempts), (Loss, num))
      case (num, _) if num < x => ((num, attempts - 1, startingModel), (Less, attempts - 1))
      case _ => ((num, attempts - 1, startingModel), (More, attempts - 1)))
