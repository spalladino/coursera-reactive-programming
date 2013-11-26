package nodescala

import scala.language.postfixOps
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import scala.async.Async.{ async, await }

object Main {

  def timeOutAfter(d: Duration): Future[String] = {
    Future.delay(d).continue(_ => "Server timeout!")
  }

  def main(args: Array[String]) {
    // TO IMPLEMENT
    // 1. instantiate the server at 8191, relative path "/test",
    //    and have the response return headers of the request
    val myServer = new NodeScala.Default(8191)
    val myServerSubscription = myServer.start("/test") { req =>
      val iter = for {
        (k, v) <- req
      } yield s"$k = $v"
      iter.iterator
    }

    // TO IMPLEMENT
    // 2. create a future that expects some user input `x`
    //    and continues with a `"You entered... " + x` message
    val userInterrupted: Future[String] = {
      Future.userInput("Enter a message: ").continue { x => s"You entered... ${x.get}" }
    }

    // TO IMPLEMENT
    // 3. create a future that completes after 20 seconds
    //    and continues with a `"Server timeout!"` message
    val timeOut: Future[String] = timeOutAfter(20 seconds)

    // TO IMPLEMENT
    // 4. create a future that completes when either 10 seconds elapse
    //    or the user enters some text and presses ENTER
    val terminationRequested: Future[String] = {
      Future.any(List(timeOutAfter(10 seconds), userInterrupted))
    }

    // TO IMPLEMENT
    // 5. unsubscribe from the server
    terminationRequested onSuccess {
      case msg => {
        println(s"Unsubscribing: $msg")
        myServerSubscription.unsubscribe
      }
    }
  }

}