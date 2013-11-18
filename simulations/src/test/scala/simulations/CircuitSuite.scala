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
    demux(in, controls, out)
    
    def assertValues(inSignal: Boolean, controlsSignals: List[Boolean], expectedSignals: List[Boolean]) {
      in.setSignal(inSignal)
      (controls, controlsSignals).zipped.map((c,s) => c.setSignal(s))
      run
      
      (out, expectedSignals, (out.length-1).to(0).by(-1)).zipped.map((o,s,i) => assert(o.getSignal === s, s"in $inSignal, controls $controlsSignals, output $i expected $s but was ${o.getSignal}"))
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
    demux(in, List(c0), List(out1, out0))
    
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
    val tester = new DemuxTester(in, List(c0), List(out1, out0))
    
    tester.assertValues(false, List(false), List(false, false))
    tester.assertValues(false, List(true),  List(false, false))
    tester.assertValues(true,  List(false), List(false, true))
    tester.assertValues(true,  List(true),  List(true,  false))
  }
  
  test("demux 2 control wire example with tester") {
    val in, c0, c1, out0, out1, out2, out3 = new Wire
    val tester = new DemuxTester(in, List(c1, c0), List(out3, out2, out1, out0))
    
    tester.assertValues(false, List(false, false), List(false, false, false, false))
    tester.assertValues(false, List(true,  false), List(false, false, false, false))
    tester.assertValues(false, List(true,  true),  List(false, false, false, false))
    tester.assertValues(false, List(false, true),  List(false, false, false, false))
    
    tester.assertValues(true, List(false, false), List(false, false, false, true))
    tester.assertValues(true, List(false, true),  List(false, false, true, false))
    tester.assertValues(true, List(true,  false), List(false, true, false, false))
    tester.assertValues(true, List(true,  true),  List(true, false, false, false))
  }
  
  // Feedback on how to write DRYer tests is most welcome!
  test("demux 3 control wire example with tester") {
    val in, c0, c1, c2, out0, out1, out2, out3, out4, out5, out6, out7 = new Wire
    val tester = new DemuxTester(in, List(c2, c1, c0), List(out7, out6, out5, out4, out3, out2, out1, out0))
    
    tester.assertValues(false, List(false, false, false), List(false, false, false, false, false, false, false, false))
    tester.assertValues(false, List(false, true,  false), List(false, false, false, false, false, false, false, false))
    tester.assertValues(false, List(false, true,  true),  List(false, false, false, false, false, false, false, false))
    tester.assertValues(false, List(false, false, true),  List(false, false, false, false, false, false, false, false))
    
    tester.assertValues(false, List(true, false, false), List(false, false, false, false, false, false, false, false))
    tester.assertValues(false, List(true, true,  false), List(false, false, false, false, false, false, false, false))
    tester.assertValues(false, List(true, true,  true),  List(false, false, false, false, false, false, false, false))
    tester.assertValues(false, List(true, false, true),  List(false, false, false, false, false, false, false, false))
    
    tester.assertValues(true, List(false, false, false), List(false, false, false, false, false, false, false, true))
    tester.assertValues(true, List(false, false, true),  List(false, false, false, false, false, false, true, false))
    tester.assertValues(true, List(false, true,  false), List(false, false, false, false, false, true, false, false))
    tester.assertValues(true, List(false, true,  true),  List(false, false, false, false, true, false, false, false))
    
    tester.assertValues(true, List(true, false, false), List(false, false, false, true, false, false, false, false))
    tester.assertValues(true, List(true, false, true),  List(false, false, true, false, false, false, false, false))
    tester.assertValues(true, List(true, true,  false), List(false, true, false, false, false, false, false, false))
    tester.assertValues(true, List(true, true,  true),  List(true, false, false, false, false, false, false, false))

  }
  

}
