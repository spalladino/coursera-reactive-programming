package suggestions

import scala.swing.Reactions.Reaction
import scala.swing.Reactions.StronglyReferenced
import scala.swing.event.Event

package object gui {

  object Reaction {
    def apply(r: Reaction) = new Reaction with StronglyReferenced {
      def apply(event: Event): Unit = r(event)
      def isDefinedAt(event: Event): Boolean = r.isDefinedAt(event)
    }
  }

}