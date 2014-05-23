import fr.enst.plnc2014.td1.TD1._
import fr.enst.plnc2014.td1.TD1.ComplexImplicit._

isOdd(1)
isOdd(6)
isOdd(-1)
isOdd(-6)

val empty = Nil
empty.any( (_ : Nothing) => false )
empty.all( (_ : Nothing) => false )

val z = Complex(3, 5)
val x = 1 : Complex
x + z
math.abs(-1)

val queens = (1,1) :: (3,2) :: Nil
isValidQueenPosition(3, queens)(3)
var l = List[(Int, Int)]()
l ::= (1, 2)
solveQueens(1, l :::= _)

