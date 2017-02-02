package Rational

/**
  * Created by А.Скрипкин on 30.01.2017.
  */
case class Ratio(a :Int, b :Int)(implicit num :Numeric[Ratio]) {

    import Rational.RichInt._

    require(b>0, "Denominator cannot be negative.")

    override def toString: String = if(1==b) a.toString else a.toString + "/" + b.toString

    final def norm :Ratio = {
      val g = a gcd b
      Ratio(a/g, b/g)
    }

    final def abs = Ratio(a.abs, b)

    final def invert :Ratio = Ratio(if(a > 0) b else -b, a.abs)

    final def unary_- :Ratio = num.negate(this)

    final def +(r :Ratio) :Ratio = num.plus(this, r)

    final def +(i :Int) :Ratio = this + i

    final def -(i :Int) :Ratio = this - i

    final def *(i :Int) :Ratio = this * i

    final def /(i :Int) :Ratio = this / i

    final def -(r :Ratio) :Ratio = this + -r

    final def *(r :Ratio) :Ratio = num.times(this,r)

    final def /(r :Ratio) :Ratio = num.times(this,r.invert)

    final def pval(p :Int) :Int = a.pval(p) - b.pval(p)

    final def toChain :Seq[Int] = {
      @scala.annotation.tailrec
      def unfold(a :Int, b :Int, l :List[Int]) :Seq[Int] = if (b == 0) l else unfold(b, a % b, a / b :: l)
      unfold(a, b, List()).reverse
    }

    final def pow(n :Int) :Ratio = n match {
      case 0 => num.fromInt(1)
      case _ if n < 0 => this.invert.pow(-n)
      case _ => Ratio(a pow n, b pow n).norm
    }

    final def pabs(p :Int) :Ratio = if(0==a) num.fromInt(0) else (num.fromInt(p) pow pval(p)).invert

}

object Ratio {

    implicit object IntRatioNumeric extends Numeric[Ratio] {

        override def plus(x: Ratio, y: Ratio): Ratio = Ratio(x.a*y.b+y.a*x.b, x.b*y.b).norm

        override def minus(x: Ratio, y: Ratio): Ratio = x + -y

        override def times(x: Ratio, y: Ratio): Ratio = Ratio(x.a*y.a, x.b*y.b).norm

        override def negate(x: Ratio): Ratio = Ratio(-x.a,x.b)

        override def fromInt(x: Int): Ratio = Ratio(x,1)

        override def toInt(x: Ratio): Int = x.a/x.b

        override def toLong(x: Ratio): Long = x.a/x.b

        override def toFloat(x: Ratio): Float = x.a.toFloat / x.b

        override def toDouble(x: Ratio): Double = x.a.toDouble / x.b

        override def compare(x: Ratio, y: Ratio): Int = (x.a * y.b) compare (y.a * x.b)
    }

  final def fromChain(l :Seq[Int])(implicit num: Numeric[Ratio]) :Ratio = {
    val lr = l.reverse
    (lr.tail foldLeft num.fromInt(lr.head)) { _.invert + num.fromInt(_) }
  }
}
