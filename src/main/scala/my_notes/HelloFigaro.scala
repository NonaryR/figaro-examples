package my_notes

import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.language.{Flip, Select, Dist, Apply, Chain}
import com.cra.figaro.library.atomic.continuous.{Normal, Uniform}
import com.cra.figaro.library.atomic.discrete.Binomial
import com.cra.figaro.library.compound.If

object FollowChapterTwo {

  val sunnyToday = Flip(0.2)

  println(VariableElimination.probability(sunnyToday, true))

  val greetingToday = If(sunnyToday,
                         Select(0.6 -> "Hello, world", 0.4 -> "Universe"),
                         Select(0.2 -> "Hello, world", 0.8 -> "Nooo"))

  greetingToday.observe("Hello, world")

  println(VariableElimination.probability(sunnyToday, true))

  greetingToday.unobserve()

  val sunnyTomorrow = If(sunnyToday, Flip(0.8), Flip(0.2))

  val greetingTomorrow = If(sunnyTomorrow,
                         Select(0.6 -> "Hello, world", 0.4 -> "Universe"),
                         Select(0.2 -> "Hello, world", 0.8 -> "Nooo"))

  println(VariableElimination.probability(greetingTomorrow, "Hello, world"))

  greetingToday.observe("Hello, world")

  println(VariableElimination.probability(greetingTomorrow, "Hello, world"))

  // 7 дней в неделю, из них каждый солнечный с вер-ю 0.2
  val numSunnyDaysInWeek = Binomial(7, 0.2)
  println(VariableElimination.probability(numSunnyDaysInWeek, 3))

  // среднее и дисперсия, которая квадрат стандартного отклонения
  val temperature = Normal(20, 25)
  def greaterThen30(d: Double) = d > 30
  println(Importance.probability(temperature, greaterThen30 _))

  // интервал значений с одинаковой плотностью вероятности
  val uniformTemperature = Uniform(0, 35)
  println(Importance.probability(uniformTemperature, greaterThen30 _))

  // с вероятностью 0.2 выбран процесс, представленный элементом Flip(0.6)
  // а с вероятностью 0.8 - Flip(0.2)
  val goodMood = Dist(0.2 → Flip(0.6), 0.8 → Flip(0.2))
  println(VariableElimination.probability(goodMood, true))

  val sunnyTodayProbability = Uniform(0, 0.5)
  val sunnyTodayF = Flip(sunnyTodayProbability)
  println(Importance.probability(sunnyTodayF, true))

  // известна дисперсия для нормального распрделения, но не среднее
  val tempMean = Normal(20, 9)
  val temperatureWithUnknownMean = Normal(tempMean, 100)
  println(Importance.probability(temperatureWithUnknownMean, greaterThen30 _))

  // может быть известно среднее, но не дисперсия
  val tempVariance = Select(0.5 → 80.0, 0.5 → 105.0)
  val temperatureWithUnknownVariance = Normal(tempMean, tempVariance)
  println(Importance.probability(temperatureWithUnknownVariance, greaterThen30 _))

  // Apply - первым аргументом - элемент, к которому применяется ф-я,
  // вторым аргументом - ф-я, применяемая к значениям аргумента
  val sunnyDaysInMonth = Binomial(30, 0.05)
  val monthQuality = Apply(sunnyDaysInMonth, (i: Int) ⇒ if (i > 5) "such sunny"; else if (i > 2) "not bad"; else "normal")
  println(VariableElimination.probability(monthQuality, "such sunny"))

  val teamWinsInMonth = Binomial(5, 0.4)
  val monthWins = Apply(sunnyDaysInMonth, teamWinsInMonth, (days: Int, wins: Int) ⇒ {
                          val x = days * wins
                          if (x > 10) "good"; else if (x > 5) "normal"; else "bad"})
  println(VariableElimination.probability(monthWins, "good"))

  val mood = Chain(monthQuality, (s: String) ⇒
    if (s == "such sunny") Flip(0.9); else if (s == "not bad") Flip(0.6); else Flip(0.1))
  println(VariableElimination.probability(mood, true))

  val teamMood = Chain(monthWins, (s: String) ⇒
    if (s == "good") Flip(0.9); else if (s == "normal") Flip(0.6); else Flip(0.1))
  println(VariableElimination.probability(teamMood, true))


  val goodMoodAndTeamWin = Chain(monthWins, sunnyToday, (quality: String, sunny: Boolean) ⇒
    if (sunny) {
      if (quality == "good") Flip(0.9)
      else if (quality == "normal") Flip(0.7)
      else Flip(0.4)
    } else {
      if (quality == "good") Flip(0.6)
      else if (quality == "normal") Flip(0.3)
      else Flip(0.05)
    })
  println(VariableElimination.probability(goodMoodAndTeamWin, true))

  // Apply → map, Apply(Flip(0.2), (b: Boolean) ⇒ !b) == Flip(0.2).map(!_)
  // Chain → flatMap, Chain(Uniform(0, 0.05), (d: Double) ⇒ Flip(d)) == Uniform(0, 0.05).flatMap(Flip(_))


  // Условия!

  println(VariableElimination.probability(sunnyDaysInMonth, 2))
  sunnyDaysInMonth.setCondition((i: Int) ⇒ i > 3) // таких дней не меньше 3х
  println(VariableElimination.probability(sunnyDaysInMonth, 2))

  println(VariableElimination.probability(mood, true))
  sunnyDaysInMonth.setCondition((i: Int) ⇒ i > 5)
  println(VariableElimination.probability(mood, true))

  sunnyDaysInMonth.addCondition((i: Int) ⇒ i % 3 == 2)
  println(VariableElimination.probability(mood, true))

  sunnyDaysInMonth.removeConditions()
  println(VariableElimination.probability(mood, true))

}
