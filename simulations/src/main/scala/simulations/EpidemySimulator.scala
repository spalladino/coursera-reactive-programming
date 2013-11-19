package simulations

import math.random
import scala.util.Random.shuffle

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt
  def roll(p: Float) = (random < p)
  
  def withProb(p : Float)(action: => Unit) {
    if (roll(p)) action
  }
  
  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    val transmissibility : Float = 0.4f
    val mortality : Float = 0.25f
    val prevalence : Float = 0.01f
    
    val sickTime : Int = 6
    val deathTime : Int = 14
    val immunityTime : Int = 16
    val recoveryTime : Int = 18
    
    val airTraffic : Boolean = false
    val reducedMobility : Boolean = false
    val vaccination : Boolean = false
    
    val airTrafficRate : Float = if (airTraffic) 0.01f else 0f
    val vaccinationRate : Float = if (vaccination) 0.05f else 0f
    val mobilityRate : Float = if (reducedMobility) 0.5f else 1f
    val sickMobilityRate : Float = if (reducedMobility) 0.25f else 1f
  }

  import SimConfig._

  type Coord = (Int, Int)
  
  val persons: List[Person] = (0 until population map (id => new Person(id))).toList
  
  shuffle(persons).take((vaccinationRate * population).toInt).foreach(_.vaccinated = true)
  shuffle(persons).filter(!_.vaccinated).take((prevalence * population).toInt).foreach(_.incubate)
  
  def peopleAt(c : Coord) : List[Person] = persons filter (_.pos == c)
  
  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false
    var vaccinated = false

    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    def pos() : Coord = (row, col) 
        
    def moveWaitTime() : Int = { 
      ((randomBelow(5) + 1) / (if (sick) sickMobilityRate else mobilityRate)).toInt 
    }
    
    def alive(action: => Unit) {
      if (!dead) action
    }
    
    def adjacents() : List[Coord] = {
      List(
       ((row+1) % roomRows, col), 
       ((row+roomRows-1) % roomRows, col), 
       (row, (col+1) % roomColumns), 
       (row, (col+roomColumns-1) % roomColumns)
      )
    }
    
    def moveCandidates() : List[Coord] = {
      adjacents filter (a => !peopleAt(a).exists(p => p.sick || p.dead) )
    }
    
    def moveTarget() : Option[Coord] = {
      moveCandidates match {
        case Nil => None
        case cs => Some(cs(randomBelow(cs.length)))
      }
    }
    
    def airTarget() : Coord = (randomBelow(roomRows), randomBelow(roomColumns))
    
    def incubate() {
      if (!vaccinated) {
	      infected = true
	      afterDelay(sickTime)     { sick = true }
	      afterDelay(deathTime)    { withProb(mortality){ dead = true } }
	      afterDelay(immunityTime) { alive { sick = false; immune = true } }
	      afterDelay(recoveryTime) { alive { infected = false; immune = false } }
      }
    }
    
    def enter(c : Coord) {
      val (i,j) = c; row = i; col = j
      if (!infected && peopleAt(c).exists(_.infected)) {
    	  withProb(transmissibility)(incubate)
      }
    }
    
    def move() {
      alive {
        roll(airTrafficRate) match {
          case true => enter(airTarget)
          case false => moveTarget map enter
        }
        afterDelay(moveWaitTime)(move)
      }
    }
    
    // Start the simulation!
    afterDelay(moveWaitTime)(move)
  }
}
