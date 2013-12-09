package suggestions

import language.postfixOps
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{ Try, Success, Failure }
import rx.lang.scala._
import org.scalatest._
import gui._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.collection.mutable.ListBuffer
import rx.lang.scala.subjects.ReplaySubject

@RunWith(classOf[JUnitRunner])
class WikipediaApiTest extends FunSuite {

  object mockApi extends WikipediaApi {
    def wikipediaSuggestion(term: String) = Future {
      if (term.head.isLetter) {
        for (suffix <- List(" (Computer Scientist)", " (Footballer)")) yield term + suffix
      } else {
        List(term)
      }
    }
    def wikipediaPage(term: String) = Future {
      "Title: " + term
    }
  }

  import mockApi._
  
  test("WikipediaApi should sanitize") {
    val obs = Observable("a ", "c d e")
    val values = obs.sanitized.toBlockingObservable.toList
    assert(values == List("a_", "c_d_e"))
  }

  test("WikipediaApi recovered should not throw onError") {
    val subject = ReplaySubject[String]()
    val obs = subject.recovered
    var list = new ListBuffer[String]()
    var completed = false

    val sub = obs.subscribe(
      {
        case Success(t) => { list.append(t) }
        case Failure(t) => { list.append(t.getMessage()) }
      },
      t => assert(false, s"Stream error $t"),
      () => completed = true)

    subject.onNext("a")
    subject.onNext("b")
    subject.onError(new NoSuchElementException("FAIL"))
    subject.onNext("c")
    subject.onCompleted()

    assert(list.toList == List("a", "b", "FAIL"), s"List is ${list.toList}")
    assert(completed, "expected stream to be completed")
  }

  test("WikipediaApi should time out the stream") {
    val obs = Observable.interval(2 seconds).take(4)
    val values = obs.timedOut(5).toBlockingObservable.toList
    assert(values == List(0, 1), s"Values are ${values}")
  }

  test("WikipediaApi should end the stream") {
    val obs = Observable.interval(1 seconds).take(2)
    val values = obs.timedOut(10).toBlockingObservable.toList
    assert(values == List(0, 1), s"Values are ${values}")
  }

  test("WikipediaApi should make the stream valid using sanitized") {
    val notvalid = Observable("erik", "erik meijer", "martin")
    val valid = notvalid.sanitized

    var count = 0
    var completed = false

    val sub = valid.subscribe(
      term => {
        assert(term.forall(_ != ' '))
        count += 1
      },
      t => assert(false, s"stream error $t"),
      () => completed = true)
    assert(completed && count == 3, "completed: " + completed + ", event count: " + count)
  }

  test("WikipediaApi recovered from flatMap") {
    val req = Observable(1, 2, 3)
    val ex = new Exception("MyException")
    val response = req.flatMap(num => if (num != 2) Observable(num) else Observable(ex)).recovered
    val l = response.toBlockingObservable.toList
    assert(l == List(Success(1), Failure(ex)), s"List was $l")
  }

  test("WikipediaApi should correctly use concatRecovered") {
    val requests = Observable(1, 2, 3)
    val remoteComputation = (n: Int) => Observable(0 to n)
    val responses = requests concatRecovered remoteComputation
    val sum = responses.foldLeft(0) { (acc, tn) =>
      tn match {
        case Success(n) => acc + n
        case Failure(t) => throw t
      }
    }
    var total = -1
    val sub = sum.subscribe {
      s => total = s
    }

    //total = sum.toBlockingObservable.toList.sum
    assert(total == (1 + 1 + 2 + 1 + 2 + 3), s"Sum: $total")
  }
  
}