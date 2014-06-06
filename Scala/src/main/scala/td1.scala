package fr.enst.plnc2014.td1

import scala.annotation.tailrec
import scala.language.implicitConversions

object TD1 {

    // Placer ici le code de isOdd & friends
    def isOdd(x: Int): Boolean = {
        if (x>=0) (x % 2) == 1
        else ( (-x) % 2) == 1
    }

    def isEven(x: Int): Boolean = !isOdd(x)

    implicit class ExtSeq[T](seq : Seq[T]){
        def any(check : T => Boolean) : Boolean = {
            for(t <- seq){
                if (check(t)) return true
            }
            false
        }

        def all(check : T => Boolean) : Boolean = {
            for(t <- seq){
                if (!check(t)) return false
            }
            true
        }
    }

    @tailrec
    def myWhile[T](b: => Boolean, f: => T) {
        if (b) {f; myWhile(b, f)}
    }

    implicit class ExtCond(b : => Boolean){
        def doWhile[T](f : => T){
            if(b){f; doWhile(f)}
        }
    }

    case class Complex(re: Double, im: Double){
        override def toString = {
            if(im != 0){
                if(re == 0) s"${im}i"
                else if(im>0) s"$re+${im}i"
                else s"${re}${im}i"
            } else re.toString
        }

        def reciprocal = Complex(re, -im)

        def +(that : Complex) : Complex = Complex(this.re + that.re, this.im + that.im)
        def -(that : Complex) = Complex(this.re - that.re, this.im - that.im)
        def *(that : Complex) = Complex(
            this.re*that.re - this.im*that.im,
            this.re*that.im + this.im*that.re
        )
    }

    object ComplexImplicit{
        implicit def double2Complex(d : Double) = Complex(d, 0)
        implicit def tuple2Complex(t : (Double, Double)) = Complex(t._1, t._2)
    }

    type QueenPos = List[(Int, Int)]

    def solveQueens(numberOfQueens: Int, f: List[(Int, Int)] => Unit){
        var result : List[QueenPos] = Nil

        def nextQueen(i: Int, q: QueenPos)(j : Int) = addQueen(i+1, (i, j)::q)
        def addQueen(i: Int, queens: QueenPos) {
            if(i>numberOfQueens)
                result ::= queens
            else
                (1 to numberOfQueens).filter(isValidQueenPosition(i, queens)).foreach(nextQueen(i, queens))
        }

        addQueen(1, Nil)
        result.foreach(f)
    }

    def cover(x: Int, y: Int)(queenPos : (Int, Int)) = {
        (x == queenPos._1
        || y == queenPos._2
        || math.abs(x - queenPos._1) == math.abs(y - queenPos._2)
        )
    }

    def isValidQueenPosition(x: Int, queens: QueenPos)(y: Int) = !queens.any(cover(x, y))


}

object Main extends App {

  import TD1._

  // Placer ici le code à exécuter

}
