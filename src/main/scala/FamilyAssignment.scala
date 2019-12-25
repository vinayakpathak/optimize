package optimize

import fs2._

case class FamilyAssignment(assignments: Map[Family, Int], ndays: Int) {
//  def attendance(day: Int) = assignments.filter(_._2 == day).keys.toList.foldLeft(0){case (acc, x) => acc + x.size}
  val attendance: Map[Int, Int] =
    List.range(1, ndays+1)
      .map(day => day -> assignments.filter(_._2 == day).keys.toList.foldLeft(0){case (acc, x) => acc + x.size}).toMap

  lazy val prefScore: Double = {
    assignments.foldLeft(0){case (acc, (f, a)) =>
      val c = if (a == f.pref(0)) 0
      else if (a == f.pref(1)) 50
      else if (a == f.pref(2)) 50 + 9*f.size
      else if (a == f.pref(3)) 100 + 9*f.size
      else if (a == f.pref(4)) 200 + 9*f.size
      else if (a == f.pref(5)) 200 + 18*f.size
      else if (a == f.pref(6)) 300 + 18*f.size
      else if (a == f.pref(7)) 300 + 36*f.size
      else if (a == f.pref(8)) 400 + 36*f.size
      else if (a == f.pref(9)) 500 + 36*f.size + 199*f.size
      else 500 + 36*f.size + 398*f.size
      acc + c
    }
  }

  lazy val accScore: Double = {
    val att = List.range(1, ndays+1).map(attendance)
    val res = Stream.emits(att)
      .map(_.toDouble)
      .zipWithPreviousAndNext
      .compile
      .fold(0.0){case (tot, (ndp, nd, ndn)) =>
        val ndn1 = ndn.getOrElse(nd)
        val ndp1 = ndp.getOrElse(nd)
        val c = (nd - 125)/400 * math.pow(nd, 0.5 + math.abs(nd - ndn1)/50)
//        println((ndp, nd, ndn, (nd-125)/400))
        tot + c
      }
    res
  }

  lazy val score: Double = {
//    println(prefScore + accScore)
    prefScore + accScore
  }
}
