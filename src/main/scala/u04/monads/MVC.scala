package u04.monads

import u04.monads.Optionals.Optional
import u04.monads.States.State

def mv[SM, SV, AM, AV](m1: State[SM, AM], f: AM => State[SV, AV]): State[(SM, SV), AV] =
  State: (sm, sv) =>
    val (sm2, am) = m1.run(sm)
    val (sv2, av) = f(am).run(sv)
    ((sm2, sv2), av)

@main def runMVC =
  import Monads.*, Monad.*, States.*, State.*, CounterStateImpl.*, WindowStateImpl.*
  import u03.extensionmethods.Streams.*

  def windowCreation(str: String): State[Window, Stream[Event]] = for
    _ <- setSize(300, 300)
    _ <- addButton(text = "inc", name = "IncButton")
    _ <- addButton(text = "dec", name = "DecButton")
    _ <- addButton(text = "reset", name = "ResetButton")
    _ <- addButton(text = "quit", name = "QuitButton")
    _ <- addLabel(text = str, name = "Label1")
    _ <- addTextField(name = "TextField1")
    _ <- addButton(text = "set", name = "SetButton")
    _ <- show()
    events <- eventStream()
  yield events

  def checkedParse(s: String): Optional[Int] = {
    try
      val number = s.toInt
      Optional.Just(number)
    catch
      case _: NumberFormatException => println("Non valid number"); Optional.Empty()
  }

  val controller = for
    events <- mv(get(), i => windowCreation(i.toString))
    _ <- seqN(events.map {
      case Event("IncButton", _) => mv(seq(inc(), get()), i => toLabel(i.toString, "Label1"))
      case Event("DecButton", _) => mv(seq(dec(), get()), i => toLabel(i.toString, "Label1"))
      case Event("ResetButton", _) => mv(seq(reset(), get()), i => toLabel(i.toString, "Label1"))
      case Event("SetButton", _) => for
        s <- mv(nop(), _ => getText("TextField1"))
        _ <- mv(seq(set(checkedParse(s).orElse(0)), get()), i => toLabel(i.toString, "Label1"))
      yield ()
      case Event("QuitButton", _) => mv(nop(), _ => exec(sys.exit()))
    })
  yield ()

  controller.run((initialCounter(), initialWindow))