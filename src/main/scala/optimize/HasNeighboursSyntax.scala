package optimize

trait HasNeighboursSyntax {
  implicit class HNOps[T: HasNeighbours](t: T) {
    def neighbours: List[T] = HasNeighbours[T].neighbours(t)
  }
}
