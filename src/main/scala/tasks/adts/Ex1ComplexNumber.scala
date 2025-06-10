package tasks.adts

/*  Exercise 1: 
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

object Ex1ComplexNumbers:

  trait ComplexADT:
    type Complex
    def complex(re: Double, im: Double): Complex
    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  object BasicComplexADT extends ComplexADT:

    // Change assignment below: should probably define a case class and use it?
    type Complex = (Double, Double)
    def complex(re: Double, im: Double): Complex = (re, im)
    extension (complex: Complex)
      def re(): Double = complex._1
      def im(): Double = complex._2
      def sum(other: Complex): Complex = (re() + other.re(), im() + other.im())
      def subtract(other: Complex): Complex = (re() - other.re(), im() - other.im())
      def asString(): String = complex match
        case (re, 0.0) => re.toString
        case (0.0, im) => im.toString concat "i"
        case (re, im) if im > 0 => re.toString concat " + " concat im.toString concat "i"
        case (re, im) => re.toString concat " - " concat (-im).toString concat "i"