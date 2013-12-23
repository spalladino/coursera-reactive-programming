package kvstore

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props
import kvstore.Persistence.Persisted
import kvstore.Persistence.Persist
import scala.concurrent.duration._
import akka.actor.ReceiveTimeout
import akka.actor.Cancellable
import kvstore.Replicator.Snapshot
import kvstore.Replicator.SnapshotAck

object ReplicateAction {
  def props(replica: ActorRef, id: Long, operation: Snapshot): Props = Props(new ReplicateAction(replica, id, operation))
  
  case class ReplicateActionFailed(id: Long, operation: Snapshot)
}

class ReplicateAction(replica: ActorRef, id: Long, operation: Snapshot) extends Actor {
  import context.dispatcher
  import ReplicateAction._
  
  context.setReceiveTimeout(1 second)

  val cancellable = context.system.scheduler.schedule(0 millis, 100 millis)(replica ! operation)

  def receive = {
    case msg @ SnapshotAck(_,_) => {
      cancellable.cancel()
      context.parent forward msg
    }
    case ReceiveTimeout => {
    	cancellable.cancel()
    	context.parent ! ReplicateActionFailed(id, operation)
    	context.stop(self)
    }
  }

}