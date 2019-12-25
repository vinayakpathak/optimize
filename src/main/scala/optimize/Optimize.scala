package optimize

object Optimize extends App {
  val result = SimulatedAnnealing[Int](10000, (i: Int) => i * i, 1)
    .optimize(100)
  println(result)
}
