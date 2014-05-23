/**
 * Created by guillaume on 23/05/14.
 */
trait Shout{
    val name: String
    def shout = s"..$name.."
}

trait Furry extends Shout{
    override def shout = s"soft${super.shout}"
}

object Imp extends App {

    implicit class convertToEnrichedString(s: String) {
        def double = s + s
    }

    println("foobar".double)

}