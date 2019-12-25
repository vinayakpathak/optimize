package optimize

trait HasNeighboursInstances {
  implicit def intHasNeighbours: HasNeighbours[Int] = HasNeighbours.instance(i => List(i - 1, i + 1))
}
