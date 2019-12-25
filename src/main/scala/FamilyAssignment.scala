package optimize

case class FamilyAssignment(assignments: Map[Family, Int]) {
  def attendance(day: Int) = assignments.filter(_._2 == day).keys.toList.foldLeft(0){case (acc, x) => acc + x.size}
}
