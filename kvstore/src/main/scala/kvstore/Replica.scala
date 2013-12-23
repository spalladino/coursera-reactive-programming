package kvstore

import akka.actor.{ OneForOneStrategy, Props, ActorRef, Actor }
import kvstore.Arbiter._
import scala.collection.immutable.Queue
import akka.actor.SupervisorStrategy.Restart
import scala.annotation.tailrec
import akka.pattern.{ ask, pipe }
import akka.actor.Terminated
import scala.concurrent.duration._
import akka.actor.PoisonPill
import akka.actor.OneForOneStrategy
import akka.actor.SupervisorStrategy
import akka.util.Timeout

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {
  import Replica._
  import Replicator._
  import Persistence._
  import PersistAction._
  import context.dispatcher

  var kv = Map.empty[String, String]
  // map from operations to their requestors
  var reqs = Map.empty[Long, ActorRef]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]
  // from operation id to the number of pending acks (includes local persistence)
  var pendingAcks = Map.empty[Long, Int]

  override val supervisorStrategy = OneForOneStrategy(maxNrOfRetries = 10, withinTimeRange = 1 minute) { case _ => Restart }

  val persistence = context.actorOf(persistenceProps)

  arbiter ! Join

  def receive = {
    case JoinedPrimary   => context.become(leader)
    case JoinedSecondary => context.become(replica())
  }

  def get(key: String, id: Long) {
    sender ! GetResult(key, kv.get(key), id)
  }

  def persist(key: String, valueOption: Option[String], id: Long) {
    reqs += ((id, sender))
    context.actorOf(PersistAction.props(persistence, Persist(key, valueOption, id)))
  }

  // Leader behaviour

  val leader: Receive = {
    case Get(key, id)                       => get(key, id)
    case Insert(key, value, id)             => leaderInsert(key, value, id)
    case Remove(key, id)                    => leaderRemove(key, id)
    case Persisted(key, id)                 => leaderPersisted(key, id)
    case PersistFailed(Persist(key, _, id)) => leaderPersistFailed(key, id)
    case Replicas(rs)                       => replicas(rs)
    case Replicated(key, id)                => replicateSuccess(key, id)
    case ReplicateFailed(key, id)           => replicateFailed(key, id)
    case _                                  => throw new Exception("Unkown message")
  }

  def leaderInsert(key: String, value: String, id: Long) {
    kv = kv.+((key, value))
    leaderOperation(key, Some(value), id)
  }

  def leaderRemove(key: String, id: Long) {
    kv = kv.-(key)
    leaderOperation(key, None, id)
  }

  def leaderPersisted(key: String, id: Long) {
    operationSuccess(key, id)
  }

  def leaderPersistFailed(key: String, id: Long) {
    operationFailed(key, id)
  }

  def leaderOperation(key: String, valueOpt: Option[String], id: Long) {
    persist(key, valueOpt, id)
    replicate(key, valueOpt, id)
    pendingAcks += ((id, replicators.size + 1))
  }

  def replicate(key: String, valueOpt: Option[String], id: Long) {
    replicators.foreach(r => r ! Replicate(key, valueOpt, id))
  }

  def replicateSuccess(key: String, id: Long) {
    operationSuccess(key, id)
  }
  
  def replicateFailed(key: String, id: Long) {
    operationFailed(key, id)
  }

  def operationSuccess(key: String, id: Long) {
    (pendingAcks.get(id)) match {
      case Some(1) => {
        reqs.get(id).get ! OperationAck(id)
        reqs -= (id)
      }
      case Some(remaining) => {
        pendingAcks += ((id, remaining - 1))
      }
      case None =>
    }
  }

  def operationFailed(key: String, id: Long) {
    pendingAcks -= (id)
    reqs.get(id).foreach(r => r ! OperationFailed(id))
    reqs -= (id)
  }

  def replicas(rs: Set[ActorRef]) {
    replicators = rs.-(self).map(r => context.actorOf(Replicator.props(r)))
  }

  // Replica behaviour

  def replica(expectedSeq: Long = 0): Receive = {
    case Get(key, id) => get(key, id)
    case Snapshot(key, value, seq) if seq == expectedSeq => storeSnapshot(key, value, seq)
    case Snapshot(key, value, seq) if seq < expectedSeq => sender ! SnapshotAck(key, seq)
    case Snapshot(key, value, _) =>
    case Persisted(key, id) => replicaPersisted(key, id)
    case PersistFailed(Persist(key, _, id)) => replicaPersistFailed(key, id)
    case _ => throw new Exception("Unkown message")
  }

  def storeSnapshot(key: String, valueOption: Option[String], seq: Long) {
    valueOption match {
      case Some(value) => kv += ((key, value))
      case None        => kv -= (key)
    }

    persist(key, valueOption, seq)
    context.become(replica(seq + 1))
  }

  def replicaPersisted(key: String, id: Long) {
    reqs.get(id).get ! SnapshotAck(key, id)
    reqs -= (id)
  }

  def replicaPersistFailed(key: String, id: Long) {
    reqs -= (id)
  }

}
