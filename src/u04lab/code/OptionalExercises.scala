package u04lab.code

import u04lab.code.Lists.List._
import Lists._
import u04lab.code.List.allMatch

  object List{
    def apply[A](elements:A*):List[A] = {
      var l: List[A] = nil
      elements foreach (e => l = append(l, Cons(e, nil)))
      l
    }

    @annotation.tailrec
    def allMatch[A](list: List[A])(elem:A):Boolean = list match {
      case Cons(head, tail) if head == elem => allMatch(tail)(head)
      case Nil() => true
      case _ => false
    }
  }


  object sameTeacher{
    def unapply(courses: List[Course]):Option[String] = map(courses)(c => c.teacher) match {
      case Cons(h, t) if allMatch(t)(h) => Some(h)
      case _ => Option.empty
    }
  }

  object OptionalExercises extends App {
    val cPPS = Course("PPS","Viroli")
    val cPCD = Course("PCD","Ricci")
    val cSDR = Course("SDR","D'Angelo")

    val c1 = Course("SO", "Bianchi")
    val c2 = Course("WEB", "Bianchi")
    val c3 = Course("MDP", "Bianchi")


    val courses = List(cPPS, cPCD, cSDR)
    val bianchiCourses = List(c1, c2, c3)
    courses match {
      case sameTeacher(t) => println(s"$courses have same teacher $t")
      case _ => println(s"$courses have different teacher")
    }

    bianchiCourses match {
      case sameTeacher(t) => println(s"$bianchiCourses have same teacher $t")
      case _ => println(s"$courses have different teacher")
    }

  }

