package u04.drawmynumber

import u04.monads.Monads.*
import u04.monads.States.State
import u04.monads.WindowStateImpl
import WindowStateImpl.*
import u04.monads.Event
import DrawMyNumberStateImpl.{Attempts, getTopBoundAndMaxAttempts, guess, initialState, nop}
import u04.monads.Monads.Monad.seqN
import u04.drawmynumber.Result.*

@main def runDrawMyNumber =
  import u03.extensionmethods.Streams.*

  def mv[SM, SV, AM, AV](m1: State[SM, AM], f: AM => State[SV, AV]): State[(SM, SV), AV] =
    State: (sm, sv) =>
      val (sm2, am) = m1.run(sm)
      val (sv2, av) = f(am).run(sv)
      ((sm2, sv2), av)

  def windowCreation(topBound: Int, attempts: Attempts): State[Window, Stream[Event]] = for
    _ <- setSize(400, 125)
    _ <- addLabel(text = "You have " + attempts + " attempts. Number is between 0 and " + topBound, name = "Label1")
    _ <- addTextField(name = "GuessField")
    _ <- addButton(text = "guess", name = "GuessButton", eventInput = "GuessField")
    _ <- show()
    events <- eventStream()
  yield events

  val controller = for
    events <- mv(getTopBoundAndMaxAttempts, (x, y) => windowCreation(x, y))
    _ <- seqN(events.map {
      case Event("GuessButton", x) => mv(guess(x.toInt), res => toLabel(res match
        case (Win, attempts) => "You won with " + attempts + " attempts left. Guess to play again"
        case (Loss, number) => "You loss. Number was " + number + ". Guess to play again"
        case (OutOfBound, topBound) => "Number must be between 0 and " + topBound
        case (Less, attempts) => "Number is less than that. Attempts left: " + attempts
        case (More, attempts) => "Number is more than that. Attempts left:" + attempts,
        "Label1"))
    })
  yield ()

  controller.run((initialState(10, 3), initialWindow))
