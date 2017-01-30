package Rational

/**
  * Created by А.Скрипкин on 30.01.2017.
  */
object RichInt {

  implicit class IntFunc(a :Int) {

    @scala.annotation.tailrec
    final def gcd(b: Int) :Int =  if (0 == b) a.abs else b gcd a % b

    final def lcm(b :Int) :Int = (a*b).abs / (a gcd b)

    final def +(r :Ratio) :Ratio = Ratio(a,1) + r
    final def -(r :Ratio) :Ratio = Ratio(a,1) - r
    final def *(r :Ratio) :Ratio = Ratio(a,1) * r
    final def /(r :Ratio) :Ratio = Ratio(a,1) / r

    final def isPrime :Boolean = a.abs == 2 || !(2 to Math.sqrt(a.abs).toInt + 1).exists(a % _ == 0)

    final def pval(p :Int) :Int = {
      @scala.annotation.tailrec
      def loop(cnt :Int, rst :Int) :Int = if(rst % p > 0) cnt else loop(cnt+1, rst/p)
      if (!p.isPrime) throw new IllegalArgumentException("Pvaluation possible only for prime.")
      loop(0,a.abs)
    }
  }

}
