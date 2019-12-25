package optimize

trait HasNeighbours[T] {
  def neighbours(t: T): List[T]
}

object HasNeighbours {
  def apply[T](implicit ev: HasNeighbours[T]): HasNeighbours[T] = ev

  def instance[T](n: T => List[T]) = new HasNeighbours[T] {
    override def neighbours(t: T): List[T] = n(t)
  }
}
