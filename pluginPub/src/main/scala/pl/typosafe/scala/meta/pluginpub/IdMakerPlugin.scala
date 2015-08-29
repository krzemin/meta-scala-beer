package pl.typosafe.scala.meta.pluginpub

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent


class IdMakerPlugin(val global: Global) extends Plugin {

  import global._

  override val name = "idmaking"
  override val components: List[PluginComponent] = List(IdMakerComponent)
  override val description: String = "using id from passports"

  private object IdMakerComponent extends PluginComponent {
    override val global: Global = IdMakerPlugin.this.global

    override def newPhase(prev: Phase): Phase = new GlobalPhase(prev) {
      override def apply(unit: CompilationUnit): Unit = {

        if(unit.toString() == "Customer.scala") {
          val x = unit.body

        }
      }

      override def name: String = "idmakingphase"
    }

    override val runsAfter: List[String] = List("parser")
    override val phaseName: String = "idmakingphase"
  }


}
