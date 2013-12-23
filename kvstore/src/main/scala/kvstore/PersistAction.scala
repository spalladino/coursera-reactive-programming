package kvstore

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props
import kvstore.Persistence.Persisted
import kvstore.Persistence.Persist
import scala.concurrent.duration._
import akka.actor.ReceiveTimeout
import akka.actor.Cancellable

object PersistAction {
  def props(persistence: ActorRef, operation: Persist): Props = Props(new PersistAction(persistence, operation))
  
  case class PersistFailed(operation: Persist)
}

class PersistAction(persistence: ActorRef, operation: Persist) extends Actor {
  import context.dispatcher
  import PersistAction._
  
  context.setReceiveTimeout(1 second)

  val cancellable = context.system.scheduler.schedule(0 millis, 100 millis)(persistence ! operation)

  def receive = {
    case msg @ Persisted(key, id) => {
      cancellable.cancel()
      context.parent forward msg
    }
    case ReceiveTimeout => {
    	cancellable.cancel()
    	context.parent ! PersistFailed(operation)
    	context.stop(self)
    }
  }

}