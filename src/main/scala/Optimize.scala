package optimize

object Optimize extends App {
  val source = io.Source.fromFile("./family_data.csv")
  val families = source.getLines().toList
      .drop(1)
      .map{line =>
        val words = line.split(",").map(_.trim)
        val family = Family(words(0).toInt, words(11).toInt, words.toList.drop(1).dropRight(1).map(_.toInt))
        family
      }
  source.close()
  val source1 = io.Source.fromFile("./submission_VR_2.csv")
  val initFa = FamilyAssignment(source1.getLines().toList
    .drop(1)
      .map{line =>
        val words = line.split(",").map(_.trim)
        families(words(0).toInt) -> words(1).toInt
      }
    .toMap,
    100
  )
  source1.close()
  val res = SimulatedAnnealing[FamilyAssignment](initFa, _.score, 1)
    .optimize(10)
  res.assignments.foreach(println)
//  println(initFa.score)
}
