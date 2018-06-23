package io.chrisdavenport.http4sstaticresourceexample

import java.util.UUID
import java.time.Instant
import io.circe._
import io.circe.syntax._
import io.circe.java8.time._
import io.circe.generic.semiauto._

import cats.implicits._
import cats.effect._
import fs2.async.Ref

import org.http4s._
import org.http4s.dsl._
import org.http4s.circe._

object Conversation {
  final case class Message[A](conversationId: String, user: String, message: String, enrichment: A)
  object Message {
    implicit def messageDecoder[A : Decoder] = deriveDecoder[Message[A]]
    implicit def messageEncoder[A: Encoder] = deriveEncoder[Message[A]]
  }

  trait ConversationAlgebra[F[_]] {
    def postMessage(message: Message[Unit]): F[Unit]
    def getConversation(conversationId: String): F[Option[Vector[Message[Instant]]]]
    def conversationIds: F[List[String]]
  }

  object ConversationAlgebra {
    def impl[F[_]: Sync]: F[ConversationAlgebra[F]] = for {
      ref <- fs2.async.refOf[F, Map[String, Vector[Message[Instant]]]](Map.empty)
    } yield internalImpl[F](ref)

    private def internalImpl[F[_]: Sync](ref: Ref[F, Map[String, Vector[Message[Instant]]]]): ConversationAlgebra[F] = 
      new ConversationAlgebra[F]{
        def mapUpdate(
          map: Map[String, Vector[Message[Instant]]], 
          newMessage: Message[Instant]): Map[String, Vector[Message[Instant]]] =
          map.get(newMessage.conversationId)
          .fold(
            map + (newMessage.conversationId -> Vector(newMessage))
          )(
            vec => map + (newMessage.conversationId -> vec.:+(newMessage))
          )

        def postMessage(message: Message[Unit]): F[Unit] = for {
          instant <- Sync[F].delay(Instant.now())
          newMessage = Message[Instant](message.conversationId, message.user, message.message, instant)
          _ <- ref.modify(map => mapUpdate(map, newMessage))
        } yield  ()

        def getConversation(conversationId: String): F[Option[Vector[Message[Instant]]]] =
          ref.get.map(_.get(conversationId))

        def conversationIds: F[List[String]] = ref.get.map(_.keys.toList)
      }
  }

  object ConversationService {

    def service[F[_]: Sync](algebra: ConversationAlgebra[F]): HttpService[F] = {
      val dsl = new Http4sDsl[F]{}
      import dsl._
      HttpService[F]{
        case req @ POST -> Root / "conversation" => for {
          message <- req.decodeJson[Message[Option[Unit]]]
          newMessage = Message[Unit](message.conversationId, message.user, message.message, ())
          _ <- algebra.postMessage(newMessage)
          resp <- Ok()
        } yield resp

        case GET -> Root / "conversation" / conversationId => 
          algebra.getConversation(conversationId)
            .flatMap(
              _.fold(NotFound())(vec => Ok(vec.asJson))
            )


      }
    }
  }
  
}