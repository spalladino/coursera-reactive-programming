/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import akka.event.LoggingReceive
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /**
   * Request with identifier `id` to insert an element `elem` into the tree.
   * The actor at reference `requester` should be notified when this operation
   * is completed.
   */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /**
   * Request with identifier `id` to check whether an element `elem` is present
   * in the tree. The actor at reference `requester` should be notified when
   * this operation is completed.
   */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /**
   * Request with identifier `id` to remove the element `elem` from the tree.
   * The actor at reference `requester` should be notified when this operation
   * is completed.
   */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /**
   * Holds the answer to the Contains request with identifier `id`.
   * `result` is true if and only if the element is present in the tree.
   */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply

  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}

class BinaryTreeSet extends Actor with Stash {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  def receive = normal

  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case msg: Operation =>
      root forward msg
    case GC =>
      val newRoot = createRoot
      root ! CopyTo(newRoot)
      context become garbageCollecting(newRoot)
  }

  var pendingQueue = Queue.empty[Operation]

  /**
   * Handles messages while garbage collection is performed.
   * `newRoot` is the root of the new binary tree where we want to copy
   * all non-removed elements into.
   */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case msg: Operation =>
      stash()
    case GC =>
    case CopyFinished =>
      root = newRoot
      unstashAll()
      context.become(normal)
  }

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean = false) = Props(classOf[BinaryTreeNode], elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  def receive = normal

  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = LoggingReceive {
    case Insert(requester, id, elem)   => insert(requester, id, elem)
    case Remove(requester, id, elem)   => remove(requester, id, elem)
    case Contains(requester, id, elem) => contains(requester, id, elem)
    case CopyTo(treeNode) => {
      val pendingCount = makeCopy(treeNode)
      handleCopying(sender, pendingCount)
    }
  }

  def insert(requester: ActorRef, id: Int, value: Int): Unit = {
    value match {
      case value if value < elem =>
        delegateInsert(Left, requester, id, value)
      case value if value > elem =>
        delegateInsert(Right, requester, id, value)
      case value if removed =>
        removed = false
        requester ! OperationFinished(id)
      case value =>
        requester ! OperationFinished(id)
    }
  }

  def delegateInsert(pos: Position, requester: ActorRef, id: Int, value: Int): Unit = {
    subtrees.get(pos) match {
      case Some(node) =>
        node forward Insert(requester, id, value)
      case None =>
        val actor = context.actorOf(BinaryTreeNode.props(value))
        subtrees = subtrees.updated(pos, actor)
        requester ! OperationFinished(id)
    }
  }

  def remove(requester: ActorRef, id: Int, value: Int): Unit = {
    value match {
      case value if value < elem =>
        delegateRemove(Left, requester, id, value)
      case value if value > elem =>
        delegateRemove(Right, requester, id, value)
      case value if !removed =>
        removed = true
        requester ! OperationFinished(id)
      case value =>
        requester ! OperationFinished(id)
    }
  }

  def delegateRemove(pos: Position, requester: ActorRef, id: Int, value: Int): Unit = {
    subtrees.get(pos) match {
      case Some(node) =>
        node forward Remove(requester, id, value)
      case None =>
        requester ! OperationFinished(id)
    }
  }

  def contains(requester: ActorRef, id: Int, value: Int): Unit = {
    value match {
      case value if value < elem =>
        delegateContains(Left, requester, id, value)
      case value if value > elem =>
        delegateContains(Right, requester, id, value)
      case value =>
        requester ! ContainsResult(id, !removed)
    }
  }

  def delegateContains(pos: Position, requester: ActorRef, id: Int, value: Int): Unit = {
    subtrees.get(pos) match {
      case Some(node) =>
        node forward Contains(requester, id, value)
      case None =>
        requester ! ContainsResult(id, false)
    }
  }

  def makeCopy(target: ActorRef): Int = {
    subtrees.values.foreach(subtree => subtree ! CopyTo(target))
    val count = subtrees.values.size

    removed match {
      case false => {
        target ! Insert(self, generateOpId(), elem)
        count + 1
      }
      case _ => count
    }
  }

  /**
   * `expected` is the set of ActorRefs whose replies we are waiting for,
   * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
   */
  def copying(requester: ActorRef, expectedCount: Int): Receive =  {
    case OperationFinished(_) | CopyFinished => {
      handleCopying(requester, expectedCount-1)
    }
    case _ => ???
  }
  
  def handleCopying(requester: ActorRef, expectedCount: Int) : Unit = {
     expectedCount match {
        case 0 => {
          requester ! CopyFinished
          context.stop(self)
        }
        case c => {
          context.become(copying(requester, c))
        }
     }
  }
  
  def generateOpId(): Int = 0
}
