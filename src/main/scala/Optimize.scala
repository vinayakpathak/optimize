package optimize

import java.time.LocalTime

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
  val source1 = io.Source.fromFile("./init.csv")
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
  println(s"pref score = ${initFa.prefScore}, acc score = ${initFa.accScore}")
//  val res = SimulatedAnnealing[FamilyAssignment](initFa, _.score, 1)
//    .optimize(1000)
//  val filename = s"./submission_${LocalTime.now()}.csv"
//  res.toCsv(filename)
}
