package chap04

import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.language.{Apply, Chain, Flip}
import com.cra.figaro.library.atomic.discrete.{Uniform}
import com.cra.figaro.library.compound.{*, OneOf, RichCPD}

object homeWork {

  // список карт
  val cards = List(5, 4, 3, 2, 1)
  val player1Card = Uniform(cards:_*)
  // исключаем карты первого игрока
  val player2Card = Chain(player1Card,
                          (card: Int) => Uniform(cards.filter(_ != card):_*))
  // хорошие карты → 0.9 true
  val player1Bet1 = RichCPD(player1Card,
                            OneOf(5, 4, 3) -> Flip(0.9),
                            * -> Flip(0.4))

  // хорошие карты или смотрим на ход первого игрока
  val player2Bet = RichCPD(player2Card, player1Bet1,
                             (OneOf(5, 4), *) -> Flip(0.9),
                             (*, OneOf(false)) -> Flip(0.5),
                             (*, *) -> Flip(0.1))

  // false после хода первого игрока, и хорошие карты у второго
  val player1Bet2 = Apply(player1Card, player1Bet1, player2Bet,
                          (card: Int, bet11: Boolean, bet2: Boolean) =>
    !bet11 && bet2 && (card == 5 || card == 4))

  // выиграет первый?
  val player1Gain = Apply(player1Card, player2Card, player1Bet1, player2Bet, player1Bet2,
                          (card1: Int, card2: Int, bet11: Boolean, bet2: Boolean, bet12: Boolean) =>
    if (!bet11 && !bet2) 0.0 // у обоих плохие карты - пас
    else if (bet11 && !bet2) 1.0 // хорошие карты у первого
    else if (!bet11 && bet2 && !bet12) -1.0 // плохие карты у первого и не бьет второго
    else if (card1 > card2) 2.0 // карта первого бьет карту второго
    else -2.0) // who-knows

  // a
  player1Card.observe(4)
  player1Bet1.observe(true)
  val alg1 = VariableElimination(player1Gain)
  alg1.start()
  alg1.stop()
  println("Поставить?" + alg1.mean(player1Gain))

  player1Bet1.observe(false)
  val alg2 = VariableElimination(player1Gain)
  alg2.start()
  alg2.stop()
  println("Пасовать?" + alg2.mean(player1Gain))
  player1Card.unobserve()
  player2Card.unobserve()

  // b
  player2Card.observe(3)
  player1Bet1.observe(true)
  player2Bet.observe(true)
  val alg3 = VariableElimination(player1Gain)
  alg3.start()
  alg3.stop()
  println("Поставить?" + alg3.mean(player1Gain))

  player2Bet.observe(false)
  val alg4 = VariableElimination(player1Gain)
  alg4.start()
  alg4.stop()
  println("Пассовать?" + alg4.mean(player1Gain))

}
