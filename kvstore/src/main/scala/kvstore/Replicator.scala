package kvstore

import akka.actor.Props
import akka.actor.Actor
import akka.actor.ActorRef
import scala.concurrent.duration._
import akka.actor.Cancellable
import kvstore.ReplicateAction.ReplicateActionFailed

object Replicator {
  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)
  case class ReplicateFailed(key: String, id: Long)
  
  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  case class SnapshotAck(key: String, seq: Long)
  
  def props(replica: ActorRef): Props = Props(new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor {
  import Replicator._
  import Replica._
  import context.dispatcher
  
  var _seqCounter = 0L
  def nextSeq = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }
  
  def receive: Receive = {
    
  	case msg@Replicate(key, valueOption, id) => {
      val seq = nextSeq
      context.actorOf(ReplicateAction.props(replica, id, Snapshot(key, valueOption, seq)))
    }
  	
    case SnapshotAck(key, seq) => {
      context.parent ! Replicated(key, seq)
    }
    
    case ReplicateActionFailed(id, Snapshot(key, _, seq)) => {
      context.parent ! ReplicateFailed(key, id)
    }
      
  }

}
