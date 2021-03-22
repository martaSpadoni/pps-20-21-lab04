package u04lab.code

import Optionals._
import Lists._
import Streams._
import u04lab.code.Lists.List.{append, nil}
import u04lab.code.StreamNewFunctions.fromList
import u04lab.code.Streams.Stream._

import scala.util.Random

object StreamNewFunctions{
  def fromList[A](list:List[A]):Stream[A] =  list match {
      case List.Cons(head, tail) => Stream.cons(head, fromList(tail))
      case _ => Stream.empty()
  }
}

trait PowerIterator[A] {
  def next(): Option[A]
  def allSoFar(): List[A]
  def reversed(): PowerIterator[A]
}

case class PowerIteratorsFromStream[A](private var stream: Stream[A]) extends PowerIterator[A] {
  private var pastList : List[A] = nil[A]

  override def next(): Option[A] = stream match {
    case Cons(h,tail) => stream = tail(); pastList = append(pastList, List.Cons(h(), List.nil)); Option.of(h())
    case Empty() => Option.empty
  }

  override def allSoFar(): List[A] = pastList

  override def reversed(): PowerIterator[A] = PowerIteratorsFromStream(fromList(List.reverse(pastList)))
}

trait PowerIteratorsFactory {

  def incremental(start: Int, successive: Int => Int): PowerIterator[Int]
  def fromList[A](list: List[A]): PowerIterator[A]
  def randomBooleans(size: Int): PowerIterator[Boolean]
}

class PowerIteratorsFactoryImpl extends PowerIteratorsFactory {

  override def incremental(start: Int, successive: Int => Int): PowerIterator[Int] = PowerIteratorsFromStream(iterate(start)(successive))

  override def fromList[A](list: List[A]): PowerIterator[A] = PowerIteratorsFromStream(StreamNewFunctions.fromList(list))

  override def randomBooleans(size: Int): PowerIterator[Boolean] = PowerIteratorsFromStream(take(generate(Random.nextBoolean()))(size))
}
