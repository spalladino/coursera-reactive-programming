package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }
  
  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 3")
  }
  
  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 3")
  }

  class DemuxTester(in: Wire, controls: List[Wire], out: List[Wire]) {
    def assertValues(inSignal: Boolean, controlsSignals: List[Boolean], expectedSignals: List[Boolean]) {
      in.setSignal(inSignal)
      (controls, controlsSignals).zipped.map((c,s) => c.setSignal(s))
      run
      
      (out, expectedSignals).zipped.map((o,s) => assert(o.getSignal === s))
    }
  }
  
  test("demux 0 control wires example") {
    val in, out0 = new Wire
    demux(in, List(), List(out0))
    
    in.setSignal(false); run
    assert(out0.getSignal === false, "demux 0")
    
    in.setSignal(true); run
    assert(out0.getSignal === true, "demux 1")
  }
  
  test("demux 1 control wire example") {
    val in, c0, out0, out1 = new Wire
    demux(in, List(c0), List(out0, out1))
    
    in.setSignal(false); c0.setSignal(false); run
    assert(out0.getSignal === false, "demux in0=0 c0=0 => out0=0")
    assert(out1.getSignal === false, "demux in0=0 c0=0 => out1=0")
    
    in.setSignal(false); c0.setSignal(true); run
    assert(out0.getSignal === false, "demux in0=0 c0=1 => out0=0")
    assert(out1.getSignal === false, "demux in0=0 c0=1 => out1=0")
    
    in.setSignal(true); c0.setSignal(false); run
    assert(out0.getSignal === true,  "demux in0=1 c0=0 => out0=1")
    assert(out1.getSignal === false, "demux in0=1 c0=0 => out1=0")
    
    in.setSignal(true); c0.setSignal(true); run
    assert(out0.getSignal === false, "demux in0=1 c0=1 => out0=0")
    assert(out1.getSignal === true,  "demux in0=1 c0=1 => out1=1")
  }
  
  test("demux 1 control wire example with tester") {
    val in, c0, out0, out1 = new Wire
    demux(in, List(c0), List(out0, out1))
    val tester = new DemuxTester(in, List(c0), List(out0, out1))
    
    tester.assertValues(false, List(false), List(false, false))
    tester.assertValues(false, List(true),  List(false, false))
    tester.assertValues(true,  List(false), List(true, false))
    tester.assertValues(true,  List(true),  List(false, true))
  }
  
  test("demux 2 control wire example with tester") {
    val in, c0, c1, out0, out1, out2, out3 = new Wire
    demux(in, List(c0, c1), List(out0, out1, out2, out3))
    val tester = new DemuxTester(in, List(c0, c1), List(out0, out1, out2, out3))
    
    tester.assertValues(false, List(false, false), List(false, false, false, false))
    tester.assertValues(false, List(true,  false), List(false, false, false, false))
    tester.assertValues(false, List(true,  true),  List(false, false, false, false))
    tester.assertValues(false, List(false, true),  List(false, false, false, false))
    
    // TODO: Set values appropriately
    tester.assertValues(true, List(false, false), List(false, false, false, false))
    tester.assertValues(true, List(true,  false), List(false, false, false, false))
    tester.assertValues(true, List(true,  true),  List(false, false, false, false))
    tester.assertValues(true, List(false, true),  List(false, false, false, false))
  }
  

}
