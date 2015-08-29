package pl.typosafe.scala.meta.pub

import pl.typosafe.scala.meta.pub.menu._

import scala.collection.SortedMap
import scala.concurrent.duration.Duration
import scala.concurrent.duration._

object Pub {

  final class Bartender private[Pub] () {

    private var orders: Seq[Drink] = Nil

    def order(drink: Drink): Unit = {
      Pub.prepare(drink)
      orders = drink +: orders
    }

    def chat(time: Duration) = Thread.sleep(time.toMillis)

    private[pub] def wholeOrder: Seq[Drink] = orders
  }

  def serves: Map[Drink, Duration] = Map(
    Ale -> 40.millis,
    Porter -> 60.millis,
    Milk -> 20.millis
  )

  private final def prepare(drink: Drink): Unit = {
    val timeToPrepare = serves(drink).toMillis
    Thread.sleep(timeToPrepare)
  }

  final def askBartender() = new Bartender

  def isAlcoholic(d: Drink) = d match {
    case _: Alcoholic => true
    case _ => false
  }

  def finalizeOrder(bt: Bartender): Seq[Drink] = bt.wholeOrder
}

case class NoSuchDrinkInMenu(drink: Drink) extends RuntimeException(s"$drink: No such drink in menu. Sorry")

class ToYoungToDrink(age: Int) extends RuntimeException(s"Having $age years is too less to order alcohol")

class UnknownAge extends RuntimeException("No id to prove age.")