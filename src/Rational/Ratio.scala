package Rational

/**
  * Created by А.Скрипкин on 30.01.2017.
  */
case class Ratio(a :Int, b :Int) {

    import Rational.RichInt._

    if (b<=0) throw new IllegalArgumentException("Denominator cannot be negative.")

    override def toString: String = if(1==b) a.toString else a.toString + "/" + b.toString

    final def norm :Ratio = {
      val g = a gcd b
      Ratio(a/g, b/g)
    }

    def this(i :Int) = this(i,1)

    final def abs = Ratio(a.abs, b)

    final def unary_- :Ratio = Ratio(-a,b)

    final def invert :Ratio = Ratio(if(a > 0) b else -b, a.abs)

    final def +(r :Ratio) :Ratio = Ratio(this.a*r.b+r.a*this.b, this.b*r.b).norm

    final def +(i :Int) :Ratio = this + Ratio(i,1)

    final def -(i :Int) :Ratio = this - Ratio(i,1)

    final def *(i :Int) :Ratio = this * Ratio(i,1)

    final def /(i :Int) :Ratio = this / Ratio(i,1)

    final def -(r :Ratio) :Ratio = this + -r

    final def *(r :Ratio) :Ratio = Ratio(a*r.a, b*r.b).norm

    final def /(r :Ratio) :Ratio = this * r.invert

    final def pval(p :Int) :Int = a.pval(p) - b.pval(p)

    final def toChain :Seq[Int] = {
      @scala.annotation.tailrec
      def unfold(a :Int, b :Int, l :List[Int]) :Seq[Int] = if (b == 0) l else unfold(b, a % b, a / b :: l)
      unfold(a, b, List()).reverse
    }

}

object Ratio {

  def apply(a: Int): Ratio = new Ratio(a, 1)

  final def fromChain(l :Seq[Int]) :Ratio = {
    val lr = l.reverse
    (lr.tail foldLeft Ratio(lr.head)) { _.invert + _ }
  }
}
