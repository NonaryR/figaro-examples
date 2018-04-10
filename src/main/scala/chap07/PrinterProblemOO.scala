package chap07

import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.algorithm.factored.VariableElimination

object PrinterProblemOO {
  class Printer {
    val powerButtonOn = Flip(0.95)
    val tonerLevel = Select(0.7 -> 'high, 0.2 -> 'low, 0.1 -> 'out)
    val tonerLowIndicatorOn =
      If(powerButtonOn,
         CPD(tonerLevel,
             'high -> Flip(0.2),
             'low -> Flip(0.6),
             'out -> Flip(0.99)),
         Constant(false))
    val paperFlow = Select(0.6 -> 'smooth, 0.2 -> 'uneven, 0.2 -> 'jammed)
    val paperJamIndicatorOn =
      If(powerButtonOn,
         CPD(paperFlow,
             'smooth -> Flip(0.1),
             'uneven -> Flip(0.3),
             'jammed -> Flip(0.99)),
         Constant(false))
    val state =
      Apply(powerButtonOn, tonerLevel, paperFlow,
            (power: Boolean, toner: Symbol, paper: Symbol) => {
              if (power) {
                if (toner == 'high && paper == 'smooth) 'good
                else if (toner == 'out || paper == 'out) 'out
                else 'poor
              } else 'out
            })
  }

  class Software {
    val state = Select(0.8 -> 'correct, 0.15 -> 'glitchy, 0.05 -> 'crashed)
  }

  class Network {
    val state = Select(0.7 -> 'up, 0.2 -> 'intermittent, 0.1 -> 'down)
  }

  class User {
    val commandCorrect = Flip(0.65)
  }

  class PrintExperience(printer: Printer, software: Software, network: Network, user: User) {

    val numPrintedPages =
      RichCPD(user.commandCorrect, network.state, software.state, printer.state,
          (*, *, *, OneOf('out)) -> Constant('zero),
          (*, *, OneOf('crashed), *) -> Constant('zero),
          (*, OneOf('down), *, *) -> Constant('zero),
          (OneOf(false), *, *, *) -> Select(0.3 -> 'zero, 0.6 -> 'some, 0.1 -> 'all),
          (OneOf(true), *, *, *) -> Select(0.01 -> 'zero, 0.01 -> 'some, 0.98 -> 'all))
    val printsQuickly =
      Chain(network.state, software.state,
            (network: Symbol, software: Symbol) =>
              if (network == 'down || software == 'crashed) Constant(false)
              else if (network == 'intermittent || software == 'glitchy) Flip(0.5)
              else Flip(0.9))
    val goodPrintQuality =
      CPD(printer.state,
          'good -> Flip(0.95),
          'poor -> Flip(0.3),
          'out -> Constant(false))
    val summary =
      Apply(numPrintedPages, printsQuickly, goodPrintQuality,
            (pages: Symbol, quickly: Boolean, quality: Boolean) =>
            if (pages == 'zero) 'none
            else if (pages == 'some || !quickly || !quality) 'poor
            else 'excellent)
  }

  val myPrinter = new Printer
  val mySoftware = new Software
  val myNetwork = new Network
  val me = new User
  val myExperience = new PrintExperience(myPrinter, mySoftware, myNetwork, me)

  def step1() {
    val answerWithNoEvidence = VariableElimination.probability(myPrinter.powerButtonOn, true)
    println("Prior probability the printer power button is on = " + answerWithNoEvidence)
  }

  def step2() {
    myExperience.summary.observe('poor)
    val answerIfPrintResultPoor = VariableElimination.probability(myPrinter.powerButtonOn, true)
    println("Probability the printer power button is on given a poor result = " + answerIfPrintResultPoor)
  }

  def main(args: Array[String]) {
    step1()
    step2()
  }
}
