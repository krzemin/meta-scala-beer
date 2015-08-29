package pl.typosafe.scala.meta.macropub

import pl.typosafe.scala.meta.pub.Pub.Bartender
import pl.typosafe.scala.meta.pub.menu.Drink

import scala.reflect.macros.whitebox.Context

object MacroPub {

  def pubMacro(c: Context)(makeOrder: c.Expr[(Bartender) => Unit]): c.Expr[Seq[Drink]] = {
    import c.universe._

    val prefixTree = c.prefix.tree
    val Apply(_, List(createCustomerAST)) = prefixTree

    val transformer = new Transformer {

      override def transform(t: Tree): Tree = t match {
        case btOrder@Apply(Select(_, TermName("order")), args) =>
          val drink = args.head
          val tt = q"""
              def age = Customer.getAge($createCustomerAST)
              if(age < 18 && Pub.isAlcoholic($drink))
                throw new ToYoungToDrink(age)
              else
                try{
                  $btOrder
                } catch {
                  case _: Throwable => throw new NoSuchDrinkInMenu($drink)
                }

           """
          c.typecheck(tt)
        case _ =>
          super.transform(t)
      }
    }

    val liftedMakeOrder = transformer.transform(makeOrder.tree)

    c.Expr(
      q"""val bt = Pub.askBartender()
         $liftedMakeOrder(bt)
         Pub.finalizeOrder(bt)""")
  }
}

