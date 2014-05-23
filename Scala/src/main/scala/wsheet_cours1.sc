Some(5)
None
class A
class B extends A
val a = new Some[A](new A)
val b = new Some[B](new B)
val c : Some[A] = b

Some("Hello world !")
Some(3) == Some(3)

Some(3) >>= ((x: Int) => Some(x+10))

val lb = Cons(b, Nil)
lb prepend a
a :: lb
lb.::(a)
lb.prepend(a)
1 :: 2 :: 3 :: Nil

