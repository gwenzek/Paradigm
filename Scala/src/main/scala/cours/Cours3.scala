package cours

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import rx.lang.scala._

object Course3 extends App {

    // Naive recursive implementation of the Fibonacci sequence.
    def fibo(n: Int): Int =
        if (n < 2)
            1
        else
            fibo(n - 2) + fibo(n - 1)

    // Let's cook a future through a promise.
    def futureFibo_old(n: Int): Future[Int] = {
        val p = Promise[Int]()
        new Thread {
            override def run(): Unit = {
                val r = fibo(n)
                p.success(r)
            }
        }.start()
        p.future
    }

    // A simpler way of writing a future.
    def futureFibo2(n: Int): Future[Int] = Future {
        fibo(n)
    }

    // This implementation will spawn many threads, as recursive computations
    // will be started in parallel.
    def futureFibo(n: Int): Future[Int] = {
        if (n < 2)
            Future {
                1
            }
        else {
            val f1 = futureFibo(n - 1)
            val f2 = futureFibo(n - 2)
            for (n1 <- f1;
                 n2 <- f2)
            yield (n1 + n2)
        }
    }

    /* Some commented out tests that we ran throughout the course.

    println("Before")
    for (r <- futureFibo(40); s <- futureFibo(5))
      println(r + s)
    futureFibo(40) foreach (r => futureFibo(5) foreach (s => println(s + r)))
    val f40 = futureFibo(10)
    val f5 = futureFibo(5)
    for (r40 <- f40; r5 <- f5) println(s"Result: ${r40 + r5}")
    println("After")

    for (i <- 1 to 10)
         println(i)

    (1 to 10) foreach (i => println(i))

    val ii = for (i <- 1 to 10) yield (i * i)
    val ii2 = (1 to 10) map (i => i * i)
    println(ii2)

    val jj = for (i <- 1 to 5; j <- 1 to 3) yield (i, j)
    val jj2 = (1 to 5) flatMap (i => (1 to 3) map (j => (i, j)))
    println(jj2)

    for (i <- Some(3)) yield println(i)
    for (i <- None) println(i)

    println("Foobar: " + Await.result(futureFibo(35), 2.seconds))

    */

    // Infinite Fibonacci sequence by the way of an observable.
    val fibos = Observable[Int] { (subscriber: Subscriber[Int]) =>
        subscriber.onNext(1)
        subscriber.onNext(1)
        var n1 = 1
        var n2 = 1
        while (!subscriber.isUnsubscribed) {
            val n = n1 + n2
            subscriber.onNext(n)
            n1 = n2
            n2 = n
            // We could stop the infinite sequence using either onCompleted
            // or onError.
            // if (n > 100)
            //   subscriber.onCompleted()
            // (or) subscriber.onError(new RuntimeException("too big a value"))
        }
    }

    // We can zip observables. And subscribe to them giving functions to call
    // for, respectively, onNext, onError, and onNext.
    val l = Observable.from(1 to 10)
    fibos.take(10).zip(l).timestamp.subscribe(
        { n: (Long, (Int,Int)) => println(n)},
        { t: Throwable => println(s"Exception: $t")},
        { () => println("Observable is terminated")}
    )

    // Here, double is just an observable and will not emit anything until it
    // gets subscribed to.
    val double = for (n <- fibos) yield (n * 2)
    double.take(5).subscribe(println(_))

    // We can cache values for other subscribers too.
    val fibosss = fibos.take(100).cache
    println("Here")
    fibosss.drop(90).first.subscribe(println(_))
    fibosss.drop(90).first.subscribe(println(_))

    // Or publish them (older values won't be rerun).
    val fiboss = fibos.publish

    // Observable.interval(1.second).timestamp.subscribe(println(_))

    // val x = fibos.take(5).toBlockingObservable.toList
    val x = fibos.take(5).toSeq.toBlockingObservable.single
    // val x = Observable.items("a", "b").toBlockingObservable.single
    // val x = Observable.empty.toBlockingObservable.single
    println(x)

    // "scan" allows to emit intermediate results.
    fibos.take(5).scan((_: Int) + (_: Int)).subscribe(println(_))

    // We can group observables by a key, given a function to compute the key.
    // fibos.take(5): Observable[Int]
    // fibos.take(5).groupBy(_%2) = Observable[(Int, Observable[Int])]
    fibos.take(5).groupBy(_ % 2)

}