package kvstore

import akka.actor.ActorSystem
import akka.testkit.{TestProbe, ImplicitSender, TestKit}
import kvstore.Arbiter.{Replicas, JoinedSecondary, JoinedPrimary, Join}
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{BeforeAndAfterAll, FunSuite}

import org.junit.runner.RunWith  
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Step7_CustomSpec
  extends TestKit(ActorSystem("Step7CustomSpec"))
  with FunSuite
  with BeforeAndAfterAll
  with ShouldMatchers
  with ImplicitSender
  with Tools {

  override def afterAll() { system.shutdown() }

  private def randomInt(implicit n: Int = 16) = (Math.random * n).toInt

  private def randomQuery(client: Session) {
    val rnd = Math.random
    if (rnd < 0.3) client.setAcked(s"k$randomInt", s"v$randomInt")
    else if (rnd < 0.6) client.removeAcked(s"k$randomInt")
    else client.getAndVerify(s"k$randomInt")
  }
  
  private def removeQuery(client: Session) {
    client.removeAcked(s"k$randomInt")
  }

  test("case1: Random ops") {
    val arbiter = TestProbe()

    val primary = system.actorOf(
      Replica.props(arbiter.ref, Persistence.props(flaky = false)), "case1-primary")
    arbiter.expectMsg(Join)
    arbiter.send(primary, JoinedPrimary)

    val secondary = system.actorOf(
      Replica.props(arbiter.ref, Persistence.props(flaky = true)), "case1-secondary")
    arbiter.expectMsg(Join)
    arbiter.send(secondary, JoinedSecondary)

    arbiter.send(primary, Replicas(Set(primary, secondary)))

    val client = session(primary)
    for (_ <- 0 until 100) removeQuery(client)
  }

  test("case2: Random ops with 3 secondaries") {
    val arbiter = TestProbe()

    val primary = system.actorOf(
      Replica.props(arbiter.ref, Persistence.props(flaky = false)), "case2-primary")
    arbiter.expectMsg(Join)
    arbiter.send(primary, JoinedPrimary)

    val secondaries = (1 to 3).map(id =>
      system.actorOf(Replica.props(arbiter.ref, Persistence.props(flaky = true)),
        s"case2-secondary-$id"))

    secondaries foreach { secondary =>
      arbiter.expectMsg(Join)
      arbiter.send(secondary, JoinedSecondary)
    }

    val client = session(primary)
    for (i <- 0 until 100) {
      randomQuery(client)
      if      (i == 10) arbiter.send(primary, Replicas(Set(secondaries(0))))
      else if (i == 20) arbiter.send(primary, Replicas(Set(secondaries(0), secondaries(1))))
      else if (i == 30) arbiter.send(primary, Replicas(Set(secondaries(0), secondaries(1), secondaries(2))))
      else if (i == 40) arbiter.send(primary, Replicas(Set(secondaries(0), secondaries(1))))
      else if (i == 50) arbiter.send(primary, Replicas(Set(secondaries(0))))
      else if (i == 60) arbiter.send(primary, Replicas(Set()))
    }
  }

  test("case3: Random ops with multiple clusters") {
    val arbiter = TestProbe()

    val primary = system.actorOf(
      Replica.props(arbiter.ref, Persistence.props(flaky = false)), "case3-primary")
    arbiter.expectMsg(Join)
    arbiter.send(primary, JoinedPrimary)

    val cluster = (1 to 10) map { i =>
      (1 to 5).toSet map { (j: Int) =>
        val secondary = system.actorOf(
          Replica.props(arbiter.ref, Persistence.props(flaky = true)),
          s"case3-secondary-$i-$j")
        arbiter.expectMsg(Join)
        arbiter.send(secondary, JoinedSecondary)
        secondary
      }
    }

    val client = session(primary)
    for (i <- 0 until 100) {
      randomQuery(client)
      if (randomInt(10) < 3)
        arbiter.send(primary, Replicas(cluster(randomInt(10))))
    }

  }

}