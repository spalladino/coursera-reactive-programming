package nodescala

import scala.language.postfixOps
import scala.util.{ Try, Success, Failure }
import scala.collection._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.async.Async.{ async, await }
import org.scalatest._
import NodeScala._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite {

  class MyTestException extends Exception

  test("A Future should always be created") {
    val always = Future.always(517)

    assert(Await.result(always, 0 nanos) == 517)
  }

  test("A Future should never be created") {
    val never = Future.never[Int]

    try {
      Await.result(never, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }

  test("A list of Futures should be created") {
    val fs = List(Future.always(1), Future.always(2), Future.always(3))
    val sf = Future.all(fs)
    assert(Await.result(sf, 1 second) == List(1, 2, 3))
  }

  test("A list of Futures should not complete until all are completed") {
    val p1, p2, p3 = promise[Int]
    val ps = List(p1, p2, p3)
    val fs = ps.map { _.future }
    val sf = Future.all(fs)

    p3.complete(Try(3))
    p1.complete(Try(1))

    try {
      Await.result(sf, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => {
        p2.complete(Try(2))
        assert(Await.result(sf, 1 second) == List(1, 2, 3))
      }
    }
  }

  test("A list of Futures should be created with the same order as the original list") {
    val p1, p2, p3 = promise[Int]
    val ps = List(p1, p2, p3)
    val fs = ps.map { _.future }
    val sf = Future.all(fs)

    p3.complete(Try(3))
    p2.complete(Try(2))
    p1.complete(Try(1))

    assert(Await.result(sf, 1 second) == List(1, 2, 3))
  }

  test("A list of Futures should fail if the first fails") {
    val p1, p2, p3 = promise[Int]
    val ps = List(p1, p2, p3)
    val sf = Future.all(ps.map { _.future })

    p1.failure(new MyTestException)
    p2.complete(Try(2))
    p3.complete(Try(3))

    try {
      Await.result(sf, 1 second)
    } catch {
      case t: MyTestException  =>
      case t: TimeoutException => assert(false, "Future timed out")
      case _: Throwable        => assert(false, "Unexpected exception")
    }
  }

  test("A list of Futures should fail if the last fails") {
    val p1, p2, p3 = promise[Int]
    val ps = List(p1, p2, p3)
    val sf = Future.all(ps.map { _.future })

    p1.complete(Try(1))
    p2.complete(Try(2))
    p3.failure(new MyTestException)

    try {
      Await.result(sf, 1 second)
    } catch {
      case t: MyTestException  =>
      case t: TimeoutException => assert(false, "Future timed out")
      case _: Throwable        => assert(false, "Unexpected exception")
    }
  }

  test("Any should return first succeeded future before the rest complete") {
    val p1, p2, p3 = promise[Int]
    val ps = List(p1, p2, p3)
    val anyf = Future.any(ps.map { _.future })

    p1.complete(Try(1))
    assert(Await.result(anyf, 1 second) == 1)
  }

  test("Any should return first succeeded future after the rest complete") {
    val p1, p2, p3 = promise[Int]
    val ps = List(p1, p2, p3)
    val anyf = Future.any(ps.map { _.future })

    p2.complete(Try(2))

    p1.complete(Try(1))
    p3.failure(new MyTestException)

    assert(Await.result(anyf, 1 second) == 2)
  }

  test("Any should return first failed future before the rest complete") {
    val p1, p2, p3 = promise[Int]
    val ps = List(p1, p2, p3)
    val anyf = Future.any(ps.map { _.future })

    p3.failure(new MyTestException)

    try {
      Await.result(anyf, 1 second)
    } catch {
      case _: MyTestException  =>
      case _: TimeoutException => assert(false, "Future timed out")
      case _: Throwable        => assert(false, "Unexpected exception")
    }

  }

  test("Any should return first failed future after the rest complete") {
    val p1, p2, p3 = promise[Int]
    val ps = List(p1, p2, p3)
    val anyf = Future.any(ps.map { _.future })

    p3.failure(new MyTestException)

    p2.complete(Try(2))
    p1.complete(Try(1))

    try {
      Await.result(anyf, 1 second)
    } catch {
      case _: MyTestException  =>
      case _: TimeoutException => assert(false, "Future timed out")
      case _: Throwable        => assert(false, "Unexpected exception")
    }
  }

  test("Delay should wait for 1 second") {
    val d = Future.delay(1 second)

    try {
      Await.result(d, 0.6 seconds)
      assert(false)
    } catch {
      case _: TimeoutException =>
        // ok, shouldn't have finished by now, we need to wait a bit more
        Await.result(d, 0.6 seconds)
    }
  }

  test("Now should return value") {
    val p = promise[Int]
    p.success(1)

    assert(p.future.now == 1)
  }

  test("Now should return timeout if not completed") {
    val p = promise[Int]

    try { p.future.now }
    catch {
      case _: NoSuchElementException =>
      case _: Throwable              => assert(false, "Unexpected exception")
    }
  }

  test("Now should return exception if future failed") {
    val p = promise[Int]
    p.failure(new MyTestException)

    try { p.future.now }
    catch {
      case _: MyTestException =>
      case _: Throwable       => assert(false, "Unexpected exception")
    }
  }

  test("CancellationTokenSource should allow stopping the computation") {
    val cts = CancellationTokenSource()
    val ct = cts.cancellationToken
    val p = Promise[String]()

    async {
      while (ct.nonCancelled) {
        // do work
      }

      p.success("done")
    }

    cts.unsubscribe()
    assert(Await.result(p.future, 1 second) == "done")
  }

  class DummyExchange(val request: Request) extends Exchange {
    @volatile var response = ""
    val loaded = Promise[String]()
    def write(s: String) {
      response += s
    }
    def close() {
      loaded.success(response)
    }
  }

  class DummyListener(val port: Int, val relativePath: String) extends NodeScala.Listener {
    self =>

    @volatile private var started = false
    var handler: Exchange => Unit = null

    def createContext(h: Exchange => Unit) = this.synchronized {
      assert(started, "is server started?")
      handler = h
    }

    def removeContext() = this.synchronized {
      assert(started, "is server started?")
      handler = null
    }

    def start() = self.synchronized {
      started = true
      new Subscription {
        def unsubscribe() = self.synchronized {
          started = false
        }
      }
    }

    def emit(req: Request) = {
      val exchange = new DummyExchange(req)
      if (handler != null) handler(exchange)
      exchange
    }
  }

  class DummyServer(val port: Int) extends NodeScala {
    self =>
    val listeners = mutable.Map[String, DummyListener]()

    def createListener(relativePath: String) = {
      val l = new DummyListener(port, relativePath)
      listeners(relativePath) = l
      l
    }

    def emit(relativePath: String, req: Request) = this.synchronized {
      val l = listeners(relativePath)
      l.emit(req)
    }
  }

  test("Listener should serve the next request as a future") {
    val dummy = new DummyListener(8191, "/test")
    val subscription = dummy.start()

    def test(req: Request) {
      val f = dummy.nextRequest()
      dummy.emit(req)
      val (reqReturned, xchg) = Await.result(f, 1 second)

      assert(reqReturned == req)
    }

    test(immutable.Map("StrangeHeader" -> List("StrangeValue1")))
    test(immutable.Map("StrangeHeader" -> List("StrangeValue2")))

    subscription.unsubscribe()
  }

  test("Server should serve requests") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => for (kv <- request.iterator) yield (kv + "\n").toString
    }

    // wait until server is really installed
    Thread.sleep(500)

    def test(req: Request) {
      val webpage = dummy.emit("/testDir", req)
      val content = Await.result(webpage.loaded.future, 1 second)
      val expected = (for (kv <- req.iterator) yield (kv + "\n").toString).mkString
      assert(content == expected, s"'$content' vs. '$expected'")
    }

    test(immutable.Map("StrangeRequest" -> List("Does it work?")))
    test(immutable.Map("StrangeRequest" -> List("It works!")))
    test(immutable.Map("WorksForThree" -> List("Always works. Trust me.")))

    dummySubscription.unsubscribe()
  }

}




