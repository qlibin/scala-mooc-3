package reductions

import org.scalameter._
import common._

object ParallelCountChangeRunner {

  @volatile var seqResult = 0

  @volatile var parResult = 0

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 80,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val amount = 250
    val coins = List(1, 2, 5, 10, 20, 50)
    val seqtime = standardConfig measure {
      seqResult = ParallelCountChange.countChange(amount, coins)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential count time: $seqtime ms")

    def measureParallelCountChange(threshold: ParallelCountChange.Threshold): Unit = {
      val fjtime = standardConfig measure {
        parResult = ParallelCountChange.parCountChange(amount, coins, threshold)
      }
      println(s"parallel result = $parResult")
      println(s"parallel count time: $fjtime ms")
      println(s"speedup: ${seqtime / fjtime}")
    }

    measureParallelCountChange(ParallelCountChange.moneyThreshold(amount))
    measureParallelCountChange(ParallelCountChange.totalCoinsThreshold(coins.length))
    measureParallelCountChange(ParallelCountChange.combinedThreshold(amount, coins))
  }
}

object ParallelCountChange {

  /** Returns the number of ways change can be made from the specified list of
   *  coins for the specified amount of money.
   */
  def oldcountChange(money: Int, coins: List[Int]): Int = {
    if (money < 0) return 0
    if (money == 0) return 1
    if (coins.count(coin => coin <= 0) > 0) throw new IllegalArgumentException("Coins can not be negative")

    def countChange(combinationsFound: Int, sumCoinsSelected: Int, lastCoinSelected: Int): Int = {
      if (sumCoinsSelected == money) {
        combinationsFound + 1
      } else if (sumCoinsSelected < money) {
        val coinsNotLessThanTheLastSelected = coins.filter(_ >= lastCoinSelected)
        val combinationsCountForDifferentChoices = coinsNotLessThanTheLastSelected.map(coin =>
          countChange(0, sumCoinsSelected + coin, coin))
        combinationsFound + combinationsCountForDifferentChoices.sum
      } else
        combinationsFound
    }

    if (money == 0) 0 else countChange(0, 0, -1)
  }

  type Threshold = (Int, List[Int]) => Boolean

  /** In parallel, counts the number of ways change can be made from the
   *  specified list of coins for the specified amount of money.
   */
  def parCountChange(money: Int, coins: List[Int], threshold: Threshold): Int = {
    if (threshold(money, coins)) {
      countChange(money, coins)
    } else {
      if (money == 0) 1
      else if (money < 0 || coins.isEmpty) 0
      else {
        val (res1, res2) = parallel(
          parCountChange(money - coins.head, coins, threshold),
          parCountChange(money, coins.tail, threshold))
        res1 + res2
      }
    }
  }

  def split(money: Int, coins: List[Int]) = {
    ((money, coins), (money, coins))
  }

  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else {
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }

  /** Threshold heuristic based on the starting money. */
  def moneyThreshold(startingMoney: Int): Threshold = (money, coins) =>
    money <= (startingMoney * 2) / 3

  /** Threshold heuristic based on the total number of initial coins. */
  def totalCoinsThreshold(totalCoins: Int): Threshold = (money, coins) =>
    coins.length <= (totalCoins * 2) / 3


  /** Threshold heuristic based on the starting money and the initial list of coins. */
  def combinedThreshold(startingMoney: Int, allCoins: List[Int]): Threshold = (money, coins) =>
    money * coins.length <= (startingMoney * allCoins.length) / 2

}
