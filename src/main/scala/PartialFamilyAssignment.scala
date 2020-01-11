package optimize

trait PartialFamilyAssignment {
  def assignments: Map[Family, Int]
  def ndays: Int
  def attendance: Map[Int, Int]
  def prefScore: Double
  def accScore: Double
  def score: Double = prefScore + accScore
}

object PartialFamilyAssignment {
  def apply(ass: Map[Family, Int], n: Int) = new PartialFamilyAssignment {
    val assignments: Map[Family, Int] = ass

    lazy val ndays: Int = n

    lazy val attendance: Map[Int, Int] =
      List.range(1, ndays+1)
        .map(day => day -> assignments.filter(_._2 == day).keys.toList.foldLeft(0){case (acc, x) => acc + x.size}).toMap

    lazy val prefScore: Double = ???

    lazy val accScore: Double = ???
  }
}
