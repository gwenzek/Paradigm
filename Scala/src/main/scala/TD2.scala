
import rx.lang.scala._
import rx.lang.scala.schedulers.IOScheduler
import scala.io.Source
import spray.json.{JsonParser, DefaultJsonProtocol, JsValue}
import DefaultJsonProtocol._

object TD2 extends App{

    def fromURL(url: String) = Observable[Char](
        (subscriber: Subscriber[Char]) => {
            val page = Source.fromURL(url)
            while (!subscriber.isUnsubscribed && page.hasNext) {
                subscriber.onNext(page.next())
            }
        }
    )

    def getContent(url: String): Observable[String] = {
        val obsURL = fromURL(url)
        obsURL.subscribeOn(IOScheduler())
        var buffer = ""
        Observable[String]{
            (subscriber: Subscriber[String]) => {
                obsURL.subscribe(
                    { (c: Char) =>
                        if(c == '\n') {
                            subscriber.onNext(buffer)
                            buffer = ""
                        } else {buffer += c}
                    },
                    { (t: Throwable) => println(s"Exception: $t"); subscriber.onNext(buffer)},
                    { () => subscriber.onNext(buffer)}
                )
            }
        }
    }

    def getJSON(url : String): Observable[JsValue] = {
        val content = getContent(url)
        Observable[JsValue]{
            (subscriber: Subscriber[JsValue]) => {
                content.subscribe(
                    { s: String => subscriber.onNext(JsonParser(s))}
                )
            }
        }
    }

//    val url = "https://www.rfc1149.net/inf355/td-scala2.html"
//    val url = "http://google.fr"
    val url = "http://ip.jsontest.com/"

//    val obsURL = fromURL(url)
//    obsURL.subscribe(
//        { c: Char => println(c) },
//        { t: Throwable => println(s"Exception: $t") },
//        { () => println("Observable is terminated") }
//    )

//    val obsPage = getContent(url)
//    obsPage.subscribe(
//        { s: String => println(s) },
//        { t: Throwable => println(s"Exception: $t") },
//        { () => println("Observable is terminated") }
//    )

    val obsJSon = getJSON(url)
    obsJSon.subscribe( {json: JsValue => println(json.prettyPrint)} )


}