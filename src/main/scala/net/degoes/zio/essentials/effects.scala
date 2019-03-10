// Copyright(C) 2019 - John A. De Goes. All rights reserved.

package net.degoes.zio
package essentials

object effects {

  // Problem:
  // def printLn(line: String): Unit
  // Trick is from procedural effects -> describe as data structures

//  object Scope {
//    sealed trait Conzole[A] { self =>
//      def map[B](f: A => B): Conzole[B] = self flatMap
//      def flatMap[B](f: A => Conzole[B], that: Conzole[B]): Conzole[B] =
//        self match {
//          case Return(a) => f(a)
//          case PrintLine(line, next) => PrintLine(line, next flatMap that)
//          case ReadLine(next) =>
//        }
//    }
//    case class Return[A](value: A) extends Conzole[A]
//    case class PrintLine[A](line: String, next: Conzole[A]) extends Conzole
//    case class ReadLine(next: String => Conzole) extends Conzole
//    def printLine(line: String): Conzole = PrintLine(line)
//    def readLine(next: String => Conzole): Conzole = ReadLine(next)
//    def succeed[A](): Conzole[A]
//
//    val program: Conzole[String]
//    printLine("Hello") *>
//    printLine("how're you?") *>
//    readLine(input =>
//      (if(input == "good") printLine("good to hear")
//      else printLine("bad..")) *>
//      succeed(input)
//    )
//  }




  /**
   * `Console` is an immutable data structure that describes a console program
   * that may involve reading from the console, writing to the console, or
   * returning a value.
   */
  sealed trait Console[A] { self =>
    import Console._

    /**
     * Implement `flatMap` for every type of `Console[A]` to turn it into a
     * `Console[B]` using the function `f`.
     */
    final def flatMap[B](f: A => Console[B]): Console[B] = ???

    final def map[B](f: A => B): Console[B] = flatMap(f andThen (Console.succeed(_)))

    final def *>[B](that: Console[B]): Console[B] = (self zip that).map(_._2)

    final def <*[B](that: Console[B]): Console[A] = (self zip that).map(_._1)

    /**
     * Implement the `zip` function using `flatMap` and `map`.
     */
    final def zip[B](that: Console[B]): Console[(A, B)] = ???
  }
  object Console {
    final case class ReadLine[A](next: String => Console[A])      extends Console[A]
    final case class WriteLine[A](line: String, next: Console[A]) extends Console[A]
    final case class Return[A](value: () => A)                    extends Console[A]

    /**
     * Implement the following helper functions:
     */
    final val readLine: Console[String]              = ???
    final def writeLine(line: String): Console[Unit] = ???
    final def succeed[A](a: => A): Console[A]        = ???
  }

  /**
   * Using the helper functions, write a program that just returns a unit value.
   */
  val unit: Console[???] = ???

  /**
   * Using the helper functions, write a program that just returns the value 42.
   */
  val fortyTwo: Console[Int] = for {
    _ <- Console.readLine
  } yield 42

  /**
   * Using the helper functions, write a program that asks the user for their name.
   */
  val askName: Console[Unit] = ???

  /**
   * Using the helper functions, write a program that read the name of the user.
   */
  val readName: Console[String] = ???

  /**
   * Using the helper functions, write a program that greets the user by their name.
   */
  def greetUser(name: String): Console[Unit] = for {
    name <- Console.readLine
    _ <- Console.writeLine(s"Hello $name")
  } yield ()

  /***
   * Using `flatMap` and the preceding three functions, write a program that
   * asks the user for their name, reads their name, and greets them.
   */
  val sayHello: Console[Unit] =
    ???

  /**
   * Write a program that reads from the console then parse the given input into int if it possible
   * otherwise it returns None
   */
  val readInt: Console[???] = ???


  // THIS IS SIDE-Effecty, non-deterministic function, necessary to actually do the side effects.
  // Should be on top of the whole program.
  /**
   * implement the following effectful procedure, which interprets
   * the description of a given `Console[A]` into A and run it.
   */

  def unsafeRun[A](program: Console[A]): A = program match {
    case Console.Return(f) => f()
    case Console.ReadLine(next) => unsafeRun(next(scala.io.StdIn.readLine()))
    case Console.WriteLine (str, next) => println(str); unsafeRun(next)
  } // TODO razobratsa

  /**
    * Implement the `foreach` function, which iterates over the values in a list,
    * passing every value to a body, which effectfully computes a `B`, and
    * collecting all such `B` values in a list.
    */
  def collectAll[A](programs: List[Console[A]]): Console[List[A]] =
    programs.foldLeft[Console[List[A]]](Console.succeed(List[A]())) { (console, program) =>
      for {
        as <- console
        a  <- program
      } yield as :+ a
    } // TODO razobratsa. can be with zip also

  /**
   * implement the `foreach` function that compute a result for each iteration
   */
  def foreach[A, B](values: List[A])(body: A => Console[B]): Console[List[B]] =  // use map & collectAll OR foldLeft
    collectAll(values.map(body)) // TODO razobratsa


  /**
   * Using `Console.writeLine` and `Console.readLine`, map the following
   * list of strings into a list of programs, each of which writes a
   * question and reads an answer.
   */
  val questions =
    List(
      "What is your name?",
      "Where where you born?",
      "Where do you live?",
      "What is your age?",
      "What is your favorite programming language?"
    )
  val answers: List[Console[String]] = questions.map(q => {
    for {
      _ <- Console.writeLine(q)
      answer <- Console.readLine
    } yield answer
  })
  // Shorter: questions.map(q => writeline(q) *> readline)



  /**
   * Using `collectAll`, transform `answers` into a program that returns
   * a list of strings.
   */
  val answers2: Console[List[String]] = ???

  /**
   * Now using only `questions` and `foreach`, write a program that is
   * equivalent to `answers2`.
   */
  val answers3: Console[List[String]] = foreach(questions) { question =>
    Console.writeLine(question) *> Console.readLine
  } // TODO - razobratsa

  // ^^^ not used in real func systems - not scalable.
  // instead of creating operation per effect, describe procedural effect, providing unevaluated non-functional (side effecty etc.) hunk of scala code


  /**
   * Implement the methods of Thunk
   */
  class Thunk[A](val unsafeRun: () => A) {
    def map[B](ab: A => B): Thunk[B]             = ???
    def flatMap[B](afb: A => Thunk[B]): Thunk[B] = ???
    def attempt: Thunk[Either[Throwable, A]]     = ???
  }
  object Thunk {
    def succeed[A](a: => A): Thunk[A]   = ???
    def fail[A](t: Throwable): Thunk[A] = ???
  }

  /**
   * Build the version of printLn and readLn
   * then make a simple program base on that.
   */
  // safe way!:
//  def printLn(line: String): Thunk[Unit] = Thunk(() => println(line))
//  def readLn: Thunk[String]              = Thunk(() => scala.io.StdIn.readLine())

  val thunkProgram: Thunk[Unit] = ???
}
