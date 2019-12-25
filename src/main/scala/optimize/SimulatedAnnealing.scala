package optimize

import breeze.linalg.DenseVector
import breeze.stats.distributions.Multinomial

import scala.annotation.tailrec

case class SimulatedAnnealing[T: HasNeighbours](init: T,
                                                obj: T => Double,
                                                temp: Double) {
  val r = scala.util.Random

  def optimize(steps: Int): T = optimize_(init, steps)

  @tailrec
  private def optimize_(t: T, steps: Int): T = if (steps <= 0) t else optimize_(nextState(t), steps-1)

  private def nextState(t: T): T = {
    println("nextState")
    val ts = HasNeighbours[T].neighbours(t)
    println("neighbours calculated")
    val sm = ts.minBy(obj)
    val ns = if (obj(sm) < obj(t)) sm else {
      val probabilities = DenseVector(ts.map(tp => math.exp(-(obj(tp) - obj(t))/temp)).toArray)
      ts(Multinomial(probabilities).draw())
    }
//    val ns = ts.find(tp => obj(tp) < obj(t)).getOrElse{
//      val probabilities = DenseVector(ts.map(tp => math.exp(-(obj(tp) - obj(t))/temp)).toArray)
//      ts(Multinomial(probabilities).draw())
//    }
    println(obj(ns))
    ns
  }
}
