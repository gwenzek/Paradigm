package cours

import scala.concurrent.{Promise, Future}
import scala.collection.mutable
import java.util.Observable

/**
 * Created by guillaume on 27/05/14.
 */
object Cours3 extends App {

    def fibo(n: Int) : Int =
        if (n < 2) 1
        else fibo(n-2) + fibo(n-1)

    println(fibo(40))

    def futureFibo(n: Int): Future[Int] = {
        val p = Promise[Int]()
        new Thread{
            override def run(): Unit = {
                val r = fibo(n)
                p.success(r)
            }
        }.start
        p.future
    }

    println("Before")
    println(futureFibo(40))
    println("After")

    // val fibos = Observable[Int] { (subscriber : mutable.Subscriber[Int])
    //     val n = n1+n2
    //     subscriber.onNext()
    //     n1 = n2
    //     n2 = n
    //     if(n>100)
    //         subscriber.onError(new RuntimeException("too big value"))
    // }

    // fibos.subscribe(
    //     {n => println()},
    // {t: Throwable => println(s"Exception: $t")},
    // {() => println("Observable is terminated")}
    // )
}
