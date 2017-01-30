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

    final def abs = Ratio(a.abs, b)

    final def unary_- :Ratio = Ratio(-a,b)

    final def invert :Ratio = Ratio(b, a)

    final def +(r :Ratio) :Ratio = Ratio(this.a*r.b+r.a*this.b, this.b*r.b).norm

    final def -(r :Ratio) :Ratio = this + -r

    final def *(r :Ratio) :Ratio = Ratio(a*r.a, b*r.b).norm

    final def /(r :Ratio) :Ratio = this * r.invert

    final def pval(p :Int) :Int = a.pval(p) - b.pval(p)
}
