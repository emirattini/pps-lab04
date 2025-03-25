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
    private case class ComplexImpl(re: Double, im: Double)
    opaque type Complex = ComplexImpl
    def complex(re: Double, im: Double): Complex = ComplexImpl(re, im)
    extension (complex: Complex)
      def re(): Double = complex.re
      def im(): Double = complex.im
      def sum(other: Complex): Complex = (complex, other) match
        case (ComplexImpl(re1, im1), ComplexImpl(re2, im2)) => ComplexImpl(re1 + re2, im1 + im2)
      def subtract(other: Complex): Complex = sum(ComplexImpl(-other.re, -other.im))
      def asString(): String = complex match
        case ComplexImpl(re, im) if re != 0.0 && im > 0.0 => re + " + " + im + "i"
        case ComplexImpl(re, im) if re != 0.0 && im < 0.0 => re + " - " + im + "i"
        case ComplexImpl(re, im) if im > 0.0 => im + "i"
        case ComplexImpl(re, im) if im < 0.0 => im + "i"
        case _ => complex.im + ""

