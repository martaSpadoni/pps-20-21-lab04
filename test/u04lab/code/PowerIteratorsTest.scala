package u04lab.code

import Optionals._
import Lists.List._
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._

class PowerIteratorsTest {

  val factory = new PowerIteratorsFactoryImpl()

  @Test
  def testIncremental() {
    val pi = factory.incremental(5,_+2) // pi produce 5,7,9,11,13,...
    assertEquals(Option.of(5), pi.next())
    assertEquals(Option.of(7), pi.next())
    assertEquals(Option.of(9), pi.next())
    assertEquals(Option.of(11), pi.next())
    assertEquals(Cons(5, Cons(7, Cons(9, Cons(11, Nil())))), pi.allSoFar()) // elementi già prodotti
    for (i <- 0 until 10) {
      pi.next() // procedo in avanti per un po'..
    }
    assertEquals(Option.of(33), pi.next()) // sono arrivato a 33
  }

  @Test
  def testReverse(): Unit ={
    val pi = factory.incremental(1, _*2)
    for (i <- 0 until 4) {
      pi.next()
    }
    val reversed = pi.reversed()
    assertEquals(Option.of(8), reversed.next())
    assertEquals(Option.of(4), reversed.next())
    assertEquals(Option.of(2), reversed.next())
    assertEquals(Option.of(1), reversed.next())
  }

  @Test
  def testRandomBooleans(): Unit ={
    val rb = factory.randomBooleans(5)
    for(i <- 1 to 5){
      println(rb.next())
      assert(rb.next().isInstanceOf[Option[Boolean]])
    }
    assertEquals(Option.empty, rb.next())
  }

  @Test
  def testFromList(): Unit ={
    val it = factory.fromList(Cons(1, Cons(2, Cons(3, Cons(4,Nil())))))
    assertEquals(Option.of(1), it.next())
    assertEquals(Option.of(2), it.next())
    assertEquals(Option.of(3), it.next())
    assertEquals(Option.of(4), it.next())
    assertEquals(Option.empty, it.next())
  }

}