package optimize

trait HasNeighboursInstances {
  implicit def intHasNeighbours: HasNeighbours[Int] = HasNeighbours.instance(i => List(i - 1, i + 1))

  implicit def familyAssignment: HasNeighbours[FamilyAssignment] = HasNeighbours.instance{fa =>
    val famass = for {
      family <- fa.assignments.keys.toList
      day <- List.range(1, fa.ndays+1).filter{d =>
        (d != fa.assignments(family)) &&
          (fa.attendance(fa.assignments(family)) - family.size >= 125) &&
          (fa.attendance(d) + family.size <= 300)
      }
    } yield FamilyAssignment(fa.assignments + (family -> day), fa.ndays)
    famass
  }
}
