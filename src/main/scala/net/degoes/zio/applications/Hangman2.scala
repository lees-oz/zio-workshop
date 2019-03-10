package net.degoes.zio
package applications

import java.io.IOException

import scalaz.zio._
import scalaz.zio.console.{Console, putStrLn}
import scalaz.zio.random.Random

object Hangman2 extends App {

  object Game {

    case class State(name: String, word: String, guesses: Set[Char]) {
      def failures: Int = (guesses -- word.toSet).size

      def isLost: Boolean = failures > 10

      def isWon: Boolean = (word.toSet -- guesses).isEmpty
    }

    val dictionary = List("word", "verb")

    def chooseWord: ZIO[Random, Nothing, String] =
      for {
        rand <- random.nextInt(dictionary.size)
      } yield dictionary(rand)

    def game: ZIO[Environment, IOException, Unit] =
      for {
        _ <- console.putStrLn("Welcome to Hangman 2")
        _ <- console.putStrLn("Please enter your name")
        name <- console.getStrLn
        _ <- console.putStrLn("Time to play, " + name)
        word <- chooseWord
        _ <- gameLoop(State(name, word, Set()))
      } yield ()

    def gameLoop(state: State): ZIO[Console, IOException, Unit] =
      for {
        _ <- renderState(state)
        guess <- takeGuess
        state <- ZIO.succeed(state.copy(guesses = state.guesses + guess))
        _ <-
          if(state.isWon) putStrLn(s"Congrats, ${state.name}, you won!")
          else if(state.isLost) putStrLn("You lost, " + state.name)
          else console.putStrLn("Keep going " + state.name) *> gameLoop(state)
      } yield ()

    def renderState(s: State): ZIO[Console, Nothing, Unit] = {
      val word = "You're trying to guess word: " + s.word
      val guesses = "Guesses:" + s.guesses.toList.mkString(", ")
      putStrLn(List(word, guesses).mkString("\n"))
    }

    def takeGuess: ZIO[Console, IOException, Char] =
      console.putStrLn("Give your guess: ") *>
      console.getStrLn
        .map(_.toLowerCase.trim.toList) flatMap {
          case letter :: Nil => ZIO.succeed(letter)
          case _ => console.putStrLn("This is not guess, this is bullshit..") *> takeGuess
        }
  }

  override def run(args: List[String]): ZIO[Hangman2.Environment, Nothing, Int] =
    Game.game.const(0) orElse ZIO.succeed(1)
}
