package io.chrisdavenport.http4sstaticresourceexample

import cats.implicits._
import cats.effect._
import fs2.Stream
import fs2.StreamApp.ExitCode
import scala.util.Properties.envOrNone
import scala.concurrent.ExecutionContext
import org.http4s.server.blaze.BlazeBuilder
import org.http4s.server.middleware.Logger

object Server {
  def server[F[_]: Effect](implicit ec: ExecutionContext): Stream[F, ExitCode] = for {
      port <- Stream.eval(Sync[F].delay(envOrNone("HTTP_PORT").map(_.toInt).getOrElse(8080)))
      ip <- Stream.eval(Sync[F].delay(envOrNone("HTTP_IP").getOrElse("0.0.0.0")))
      conversationsAlgebra <- Stream.eval(Conversation.ConversationAlgebra.impl[F])
      baseService = Conversation.ConversationService.service[F](conversationsAlgebra) <+> StaticSite.service[F]
      finalService = Logger[F](true, false)(baseService)
      exitCode <- BlazeBuilder[F]
        .bindHttp(port, ip)
        .mountService(finalService)
        .serve
} yield exitCode
}