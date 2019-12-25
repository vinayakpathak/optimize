package optimize

trait HasNeighbours[T] {
  def neighbours(t: T): List[T]
}
