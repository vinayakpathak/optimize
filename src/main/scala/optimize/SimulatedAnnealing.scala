package optimize

import java.time.LocalTime
import java.time.format.DateTimeFormatter

import breeze.linalg.DenseVector
import breeze.stats.distributions.Multinomial

import scala.annotation.tailrec

case class SimulatedAnnealing[T: HasNeighbours](init: T,
                                                obj: T => Double,
                                                stepsTotal: Int,
                                                tempInit: Double) {
  val r = scala.util.Random

  def optimize: T = optimize_(init, stepsTotal)

  @tailrec
  private def optimize_(t: T, steps: Int): T = if (steps <= 0) t else optimize_(nextState(t, steps), steps-1)

  private def nextState(t: T, steps: Int): T = {
    println("nextState")
    val ts = HasNeighbours[T].neighbours(t)
    println("neighbours calculated")
//    val ns = {
//      val rn = r.nextInt(ts.size)
//      println(s"next neighbour's index = $rn")
//      val rns = ts(rn)
//      println(s"chosen neighbour = $rns")
//      val rd = r.nextDouble()
//      println(s"random double = $rd")
//      println(s"obj at rns = ${obj(rns)} obj at current = ${obj(t)}")
//      val temp = tempInit/math.log10(10 + stepsTotal - steps)
//      println(s"temperature = $temp")
//      val acceptProb = math.exp(-(obj(rns) - obj(t))/temp) // < 1 if rns is worse, >=1 otherwise
//      println(s"acceptProb = $acceptProb")
//      if (rd <= acceptProb) rns else t
//    }
    val sm = ts.minBy(obj)
    val ns = if (obj(sm) < obj(t)) sm else {
      val probabilities = DenseVector(ts.map(tp => math.exp(-(obj(tp) - obj(t))/tempInit)).toArray)
      ts(Multinomial(probabilities).draw())
    }
//    val ns = ts.find(tp => obj(tp) < obj(t)).getOrElse{
//      val probabilities = DenseVector(ts.map(tp => math.exp(-(obj(tp) - obj(t))/temp)).toArray)
//      ts(Multinomial(probabilities).draw())
//    }
    val score = obj(ns)
    val fn = s"./data/submission_${LocalTime.now.format(DateTimeFormatter.ISO_TIME)}_${score}.csv"
    println(s"score = $score")
    if (score < 70000) {
      ns.asInstanceOf[FamilyAssignment]
        .toCsv(fn)
      println(s"Saving into file $fn")
    }
    ns
  }
}
