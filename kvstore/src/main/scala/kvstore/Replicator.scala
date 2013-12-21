package kvstore

import akka.actor.Props
import akka.actor.Actor
import akka.actor.ActorRef
import scala.concurrent.duration._
import akka.actor.Cancellable

object Replicator {
  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Rereplicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)
  
  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  case class SnapshotAck(key: String, seq: Long)

  case class PendingAck(id: Long, requestor: ActorRef, cancellable: Cancellable)
  
  def props(replica: ActorRef): Props = Props(new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor {
  import Replicator._
  import Replica._
  import context.dispatcher
  
  // Map from sequence number to a pending ack
  var acks = Map.empty[Long, PendingAck]
  // a sequence of not-yet-sent snapshots (you can disregard this if not implementing batching)
  //var pending = Vector.empty[Snapshot]
  
  var _seqCounter = 0L
  def nextSeq = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }
  
  def receive: Receive = {
    
  	case msg@Replicate(key, valueOption, id) => {
      val seq = nextSeq
      val cancellable = context.system.scheduler.schedule(100 millis, 100 millis) { self ! Rereplicate(key, valueOption, seq) }
      acks += ((seq, PendingAck(id, sender, cancellable)))
      replica ! Snapshot(key, valueOption, seq)
    }
  	
  	case Rereplicate(key, valueOption, seq) => {
  	  replica ! Snapshot(key, valueOption, seq)
  	}
    
    case SnapshotAck(key, seq) => {
      val PendingAck(id, requestor, cancellable) = acks.get(seq).get
      cancellable.cancel()
      acks -= seq
      requestor ! Replicated(key, id)
    }
      
  }

}
