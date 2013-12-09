package suggestions

import language.postfixOps
import scala.language.postfixOps
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Success, Failure}
import suggestions.observablex.ObservableEx
import rx.lang.scala._
import org.scalatest._
import gui._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.async.Async


@RunWith(classOf[JUnitRunner])
class ObservableExTest extends FunSuite {

  test("ObservableEx should create successul observable from future") {
    var witness = 0
    var completed = false

    val promise = Promise[Int]()
    val f = promise.future
    
    val obs = ObservableEx(f)
    val sub = obs.subscribe( 
      term => { witness = term},
      t => assert(false, s"stream error $t"),
      () => completed = true
    )
   
    assert(witness == 0, "witness should not be modified yet")

    promise.complete(Success(42))
    Thread.sleep(100)
    assert(witness == 42, "witness should have been modified")
  }
  
  test("ObservableEx should create failed observable from future") {
    var fail : Throwable = null

    val promise = Promise[Int]()
    val f = promise.future
    
    val obs = ObservableEx(f)
    val sub = obs.subscribe( 
      term => assert(false, s"received term $term"),
      t => { fail = t },
      () => assert(false, "completed successfully")
    )
   
    assert(fail == null, "fail should be null")
    
    val t = new NoSuchElementException()
    promise.complete(Failure(t))
    Thread.sleep(100)
    assert(fail == t, "should have failed")
  }


}