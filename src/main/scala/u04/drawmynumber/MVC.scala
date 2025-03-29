package u04.drawmynumber

import u04.monads.Monads.*
import u04.monads.States.State
import u04.monads.WindowStateImpl
import WindowStateImpl.*
import u04.monads.Event
import DrawMyNumberStateImpl.*
import u04.monads.Monads.Monad.seqN
import u04.drawmynumber.Result.*
import u03.extensionmethods.Streams.*

@main def runDrawMyNumber =

  def windowCreation(topBound: Int, attempts: Int): State[Window, Stream[Event]] = for
    _ <- setSize(400, 125)
    _ <- addLabel(text = "You have " + attempts + " attempts. Number is between 0 and " + topBound, name = "Label1")
    _ <- addTextField(name = "GuessField")
    _ <- addButton(text = "guess", name = "GuessButton")
    _ <- show()
    events <- eventStream()
  yield events

  def mapToText(res: Any) = {
    res match
      case (Win, attempts) => "You won with " + attempts + " attempts left. Guess to play again"
      case (Loss, number) => "You loss. Number was " + number + ". Guess to play again"
      case (OutOfBound, topBound) => "Number must be between 0 and " + topBound
      case (Less, attempts) => "Number is less than that. Attempts left: " + attempts
      case (More, attempts) => "Number is more than that. Attempts left: " + attempts
  }

  import u04.monads.mv
  val controller = for
    events <- mv(getTopBoundAndAttempts, windowCreation)
    _ <- seqN(events.map {
      case Event("GuessButton", _) => for
        x <- mv(nop(), _ => getText("GuessField"))
        _ <- mv(guess(x.toInt), res => toLabel(mapToText(res), "Label1"))
      yield()
    })
  yield ()

  controller.run((initialState(100, 8), initialWindow))
