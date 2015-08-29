package pl.typosafe.scala.meta
package customer

import pl.typosafe.scala.meta.pub.ToYoungToDrink
import pl.typosafe.scala.meta.pub.menu.NoId

class Customer {
}

trait HasId {
  def ageFromId: Int
}

trait HasPassport {
  def ageFromPassport: Int

  def age = 25
}

case class LocalCustomer(age: Int) extends Customer

case class CustomerWithId(ageFromId: Int) extends Customer with HasId

case class CustomerWithPassport(ageFromPassport: Int) extends Customer with HasPassport

object Customer {
  def getAge(c: Customer) = c match {
    case withId: HasId =>
      withId.ageFromId
    case withPassport: HasPassport =>
      withPassport.ageFromPassport
    case LocalCustomer(cAge) =>
      cAge
    case _ =>
      throw NoId
  }
}
