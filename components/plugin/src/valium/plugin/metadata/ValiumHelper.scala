package valium.plugin.metadata

import scala.tools.nsc.Global

trait ValiumHelper extends ValiumInfo with ValiumDefs {
  val global: Global
}
