
abstract class Option[+T]{
    def isDefined(): Boolean
    def get(): T
    def bind[U](f : T => Option[U]) : Option[U]
    def >>=[U](f : T => Option[U]) = bind(f)
}

case class Some[+T](get: T) extends Option[T]{
    def isDefined() : Boolean = true
    def bind[U](f : T => Option[U]) = f(get)
}

case object None extends Option[Nothing]{
    val isDefined = false
    def get = sys.error("No get for none")
    def bind[U](f: (Nothing) => Option[U]): Option[U] = None
}

abstract class List[+T]{
    def head: T
    def tail: List[T]
    def isEmpty: Boolean
    // as the name '::' ends with ':' the notation is inverted
    // a :: la  == la prepend a == la.::(a) == la.prepend(a)
    def prepend[U >: T](start: U): List[U] = Cons(start, this)
    def ::[U >: T](start: U): List[U] = Cons(start, this)

    def internalToString : String =
        if(isEmpty) ""
        else if(tail.isEmpty) s"$head"
        else s"$head, ${tail.internalToString}"

    //the string representation of the list is stored in a val, and evaluated at the first call of "toString"
    override lazy val toString : String=
        if (isEmpty) "()"
        else s"($head, $internalToString)"

}

case object Nil extends List[Nothing]{
    def head = sys.error("no head")
    def tail = sys.error("no tail")
    def isEmpty = true
}

case class Cons[+T](head: T, tail: List[T]) extends List[T]{
    def isEmpty = false
}
