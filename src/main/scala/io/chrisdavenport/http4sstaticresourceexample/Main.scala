package io.chrisdavenport.http4sstaticresourceexample

import fs2.StreamApp
import fs2.Stream
import cats.effect.IO
import scala.concurrent.ExecutionContext.Implicits.global

object Main extends StreamApp[IO] {

  def stream(args: List[String], requestShutdown: IO[Unit]) : Stream[IO, StreamApp.ExitCode] = 
    Server.server[IO]


}