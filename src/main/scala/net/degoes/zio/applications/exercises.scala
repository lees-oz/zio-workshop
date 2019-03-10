// Copyright(C) 2019 - John A. De Goes. All rights reserved.

package net.degoes.zio
package applications

import java.io.IOException

import scalaz.zio._
import scalaz.zio.blocking.Blocking
import scalaz.zio.console.{Console, putStrLn}
import scalaz.zio.duration.Duration
import scalaz.zio.random.Random

object circuit_breaker extends App {

  /**
   * implement a circuit breaker mechanism
   */
  trait CircuitBreaker {

  }

  object CircuitBreaker {
    sealed trait State
    final case class Closed(maxFailure: Int)                      extends State
    final case class Open(startAt: Duration, openUntil: Duration) extends State
    final case class HalfOpen(retryUntil: Duration)               extends State

  }

  override def run(args: List[String]): ZIO[Environment, Nothing, Int] = ???
}

object hangman extends App {

  /**
    * Create a hangman game that requires the capability to perform `Console` and `Random` effects.
    */
  def myGame: ZIO[Console with Random, IOException, Unit] =
    for {
      _    <- console.putStrLn("Welcome to Purely Functional Hangman!")
      name <- getName
      _    <- console.putStrLn(s"Time to play, $name!")
      word <- chooseWord
      _    <- gameLoop(State(name, Set(), word))
    } yield ()

  case class State(name: String, guesses: Set[Char], word: String) {
    def failures: Int = (guesses -- word.toSet).size

    def playerLost: Boolean = failures > 10

    def playerWon: Boolean = (word.toSet -- guesses).isEmpty
  }

  def gameLoop(state: State): ZIO[Console, IOException, State] =
    for {
      _     <-  renderState(state)
      char  <-  getChoice
      state <-  ZIO.succeed(state.copy(guesses = state.guesses + char))
      cont  <-  if (state.playerWon)
        putStrLn(s"Congratulations, ${state.name}, you won!!!").const(false)
      else if (state.playerLost)
        putStrLn(s"Sorry, ${state.name}, you lost. Please try again!!!").const(false)
      else putStrLn(s"Keep going, ${state.name}, you still have a chance!").const(true)
      state <-  if (cont) gameLoop(state)
      else renderState(state).const(state)
    } yield state

  def renderState(state: State): ZIO[Console, Nothing, Unit] = {

    /**
      *
      *  f     n  c  t  o
      *  -  -  -  -  -  -  -
      *
      *  Guesses: a, z, y, x
      *
      */
    val word =
      state.word.toList.map(c => if (state.guesses.contains(c)) s" $c " else "   ").mkString("")

    val line = List.fill(state.word.length)(" - ").mkString("")

    val guesses = " Guesses: " + state.guesses.mkString(", ")

    val text = word + "\n" + line + "\n\n" + guesses + "\n"

    putStrLn(text)
  }

  def getChoice: ZIO[Console, IOException, Char] =
    console.putStrLn("Please guess a letter: ") *>
      console.getStrLn.map(_.toLowerCase.trim.toList).flatMap {
        case letter :: Nil => ZIO.succeed(letter)
        case _ =>
          putStrLn("You did not enter a single letter") *>
            getChoice
      }

  def getName: ZIO[Console, IOException, String] =
    console.putStrLn("Please enter your name: ") *>
      console.getStrLn

  def chooseWord: ZIO[Random, Nothing, String] =
    random.nextInt(Dictionary.length).map(index =>
      Dictionary.lift(index).getOrElse("defect")
    )

  val Dictionary = List("bob", "zopa")

  /**
    * Create a test data structure that can contain a buffer of lines (to be
    * read from the console), a log of output (that has been written to the
    * console), and a list of "random" numbers.
    */
  override def run(args: List[String]): ZIO[Environment, Nothing, Int] =
    myGame.const(0) orElse ZIO.succeed(1)
}

object parallel_web_crawler {

  final case class URL private (parsed: io.lemonlabs.uri.Url) {
    import io.lemonlabs.uri._

    final def relative(page: String): Option[URL] =
      scala.util
        .Try(parsed.path match {
          case Path(parts) =>
            val whole = parts.dropRight(1) :+ page.dropWhile(_ == '/')

            parsed.withPath(UrlPath(whole))
        })
        .toOption
        .map(new URL(_))

    def url: String = parsed.toString

    override def equals(a: Any): Boolean = a match {
      case that: URL => this.url == that.url
      case _         => false
    }

    override def hashCode: Int = url.hashCode
  }

  object URL {
    import io.lemonlabs.uri._

    def apply(url: String): Option[URL] =
      scala.util.Try(AbsoluteUrl.parse(url)).toOption match {
        case None         => None
        case Some(parsed) => Some(new URL(parsed))
      }
  }

  private val blockingPool = java.util.concurrent.Executors.newCachedThreadPool()

  def getURL(url: URL): ZIO[Blocking, Exception, String] =
    blocking.interruptible(scala.io.Source.fromURL(url.url)(scala.io.Codec.UTF8).mkString).refineOrDie{JustExceptions}


  def extractURLs(root: URL, html: String): List[URL] = {
    val pattern = "href=[\"\']([^\"\']+)[\"\']".r

    scala.util
      .Try({
        val matches = (for (m <- pattern.findAllMatchIn(html)) yield m.group(1)).toList

        for {
          m   <- matches
          url <- URL(m).toList ++ root.relative(m).toList
        } yield url
      })
      .getOrElse(Nil)
  }

  object test {
    val Home          = URL("http://scalaz.org").get
    val Index         = URL("http://scalaz.org/index.html").get
    val ScaladocIndex = URL("http://scalaz.org/scaladoc/index.html").get
    val About         = URL("http://scalaz.org/about").get

    val SiteIndex =
      Map(
        Home          -> """<html><body><a href="index.html">Home</a><a href="/scaladoc/index.html">Scaladocs</a></body></html>""",
        Index         -> """<html><body><a href="index.html">Home</a><a href="/scaladoc/index.html">Scaladocs</a></body></html>""",
        ScaladocIndex -> """<html><body><a href="index.html">Home</a><a href="/about">About</a></body></html>""",
        About         -> """<html><body><a href="home.html">Home</a><a href="http://google.com">Google</a></body></html>"""
      )

    val getURL: URL => IO[Exception, String] =
      (url: URL) =>
        SiteIndex
          .get(url)
          .fold[IO[Exception, String]](IO.fail(new Exception("Could not connect to: " + url)))(IO.effectTotal(_))

    val ScalazRouter: URL => Set[URL] =
      url => if (url.parsed.apexDomain == Some("scalaz.org")) Set(url) else Set()

    val Processor: (URL, String) => IO[Unit, List[(URL, String)]] =
      (url, html) => IO.succeed(List(url -> html))
  }

  def run(args: List[String]): ZIO[Console, Nothing, Int] =
    (for {
      _ <- putStrLn("Hello World!")
    } yield ()).fold(_ => 1, _ => 0)
}
