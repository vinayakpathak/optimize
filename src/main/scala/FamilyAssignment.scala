package optimize

import java.io.{BufferedWriter, File, FileWriter}

import fs2._

trait FamilyAssignment { self =>
  def assignments: Map[Family, Int]
  def ndays: Int
  def attendance: Map[Int, Int]
  def prefScore: Double
  def accScore: Double
  def score: Double = prefScore + accScore
  def toCsv(filename: String): Unit = {
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    val header = s"family_id,assigned_day\n"
    val lines = List(header)++assignments.toList.map{case (f,i) => (f.id, i)}.sortBy(_._1).map{case (id, i) => s"$id,$i\n"}
    for (line <- lines) {
      bw.write(line)
    }
    bw.close()
  }
  def reassign(family: Int, day: Int) = new FamilyAssignment {
    lazy val (currentFam, currentDay) = self.assignments.find(_._1.id == family).get

    lazy val assignments: Map[Family, Int] = {
      self.assignments + (currentFam -> day)
    }

    lazy val ndays: Int = self.ndays

    lazy val attendance: Map[Int, Int] = self.attendance +
      (currentDay -> (self.attendance(currentDay) - currentFam.size)) +
      (day -> (self.attendance(day) + currentFam.size))

    lazy val prefScore: Double = self.prefScore -
      FamilyAssignment.prefScoreForFamily(currentFam, currentDay) +
      FamilyAssignment.prefScoreForFamily(currentFam, day)

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
          tot + c
        }
      res
    }
  }
}

object FamilyAssignment {
  def prefScoreForFamily(family: Family, day: Int): Double = {
    if (day == family.pref(0)) 0
    else if (day == family.pref(1)) 50
    else if (day == family.pref(2)) 50 + 9*family.size
    else if (day == family.pref(3)) 100 + 9*family.size
    else if (day == family.pref(4)) 200 + 9*family.size
    else if (day == family.pref(5)) 200 + 18*family.size
    else if (day == family.pref(6)) 300 + 18*family.size
    else if (day == family.pref(7)) 300 + 36*family.size
    else if (day == family.pref(8)) 400 + 36*family.size
    else if (day == family.pref(9)) 500 + 36*family.size + 199*family.size
    else 500 + 36*family.size + 398*family.size
  }

  def apply(ass: Map[Family, Int], n: Int) = new FamilyAssignment {
    val assignments = ass
    val ndays = n
    lazy val attendance: Map[Int, Int] =
      List.range(1, ndays+1)
        .map(day => day -> assignments.filter(_._2 == day).keys.toList.foldLeft(0){case (acc, x) => acc + x.size}).toMap

    lazy val prefScore: Double = {
      assignments.foldRight(0.0){case ((f, a), acc) =>
        acc + prefScoreForFamily(f, a)
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

  }
}
//case class FamilyAssignment(assignments: Map[Family, Int], ndays: Int) {
////  def attendance(day: Int) = assignments.filter(_._2 == day).keys.toList.foldLeft(0){case (acc, x) => acc + x.size}
//  lazy val attendance: Map[Int, Int] =
//    List.range(1, ndays+1)
//      .map(day => day -> assignments.filter(_._2 == day).keys.toList.foldLeft(0){case (acc, x) => acc + x.size}).toMap
//
//  lazy val prefScore: Double = {
//    assignments.foldLeft(0){case (acc, (f, a)) =>
//      val c = if (a == f.pref(0)) 0
//      else if (a == f.pref(1)) 50
//      else if (a == f.pref(2)) 50 + 9*f.size
//      else if (a == f.pref(3)) 100 + 9*f.size
//      else if (a == f.pref(4)) 200 + 9*f.size
//      else if (a == f.pref(5)) 200 + 18*f.size
//      else if (a == f.pref(6)) 300 + 18*f.size
//      else if (a == f.pref(7)) 300 + 36*f.size
//      else if (a == f.pref(8)) 400 + 36*f.size
//      else if (a == f.pref(9)) 500 + 36*f.size + 199*f.size
//      else 500 + 36*f.size + 398*f.size
//      acc + c
//    }
//  }
//
//  lazy val accScore: Double = {
//    val att = List.range(1, ndays+1).map(attendance)
//    val res = Stream.emits(att)
//      .map(_.toDouble)
//      .zipWithPreviousAndNext
//      .compile
//      .fold(0.0){case (tot, (ndp, nd, ndn)) =>
//        val ndn1 = ndn.getOrElse(nd)
//        val ndp1 = ndp.getOrElse(nd)
//        val c = (nd - 125)/400 * math.pow(nd, 0.5 + math.abs(nd - ndn1)/50)
////        println((ndp, nd, ndn, (nd-125)/400))
//        tot + c
//      }
//    res
//  }
//
//  lazy val score: Double = {
////    println(prefScore + accScore)
//    prefScore + accScore
//  }
//
//  def toCsv(filename: String): Unit = {
//    val file = new File(filename)
//    val bw = new BufferedWriter(new FileWriter(file))
//    val header = s"family_id,assigned_day\n"
//    val lines = List(header)++assignments.toList.map{case (f,i) => (f.id, i)}.sortBy(_._1).map{case (id, i) => s"$id,$i\n"}
//    for (line <- lines) {
//      bw.write(line)
//    }
//    bw.close()
//  }
//}

