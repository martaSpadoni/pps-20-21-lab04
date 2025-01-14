package u04lab.code

trait Complex {
  def re: Double
  def im: Double
  def +(c: Complex): Complex // should implement the sum of two complex numbers..
  def *(c: Complex): Complex // should implement the product of two complex numbers
}

case class ComplexImpl(override val re:Double, override val im:Double ) extends Complex {
  override def +(c: Complex): Complex = ComplexImpl(re+c.re, im+c.im)
  override def *(c: Complex): Complex = ComplexImpl((re*c.re)-(im*c.im), (re*c.im)+(im*c.re))
}

//object Complex {
//  def apply(re:Double, im:Double):Complex = new ComplexImpl(re, im)
//}

//class ComplexImpl(override val re:Double, override val im:Double ) extends Complex {
//  override def +(c: Complex): Complex = Complex(re+c.re, im+c.im)
//  override def *(c: Complex): Complex = Complex((re*c.re)-(im*c.im), (re*c.im)+(im*c.re))
//}

object TryComplex extends App {

  val comp = ComplexImpl(1,2.5)
  println(comp.re, comp.im)
  val a = Array(ComplexImpl(10,20), ComplexImpl(1,1), ComplexImpl(7,0))
  val c = a(0) + a(1) + a(2)
  //without the explicit implementation of toString or the use of a case class,
  // when I print the object I get its ref and not the representation in the comment
  println(c, c.re, c.im) // (ComplexImpl(18.0,21.0),18.0,21.0)
  val c2 = a(0) * a(1)
  println(c2, c2.re, c2.im) // (ComplexImpl(-10.0,30.0),-10.0,30.0)

  val c3 = ComplexImpl(2,3)
  val c4 = ComplexImpl(2,3)
  println(if (c3==c4) "equals" else "not equals")

}


/** Hints:
  * - implement Complex with a ComplexImpl class, similar to PersonImpl in slides
  * - check that equality and toString do not work
  * - use a case class ComplexImpl instead, creating objects without the 'new' keyword
  * - check equality and toString now
  */