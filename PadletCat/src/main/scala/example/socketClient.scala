package example

import java.net.InetSocketAddress
import akka.io.{IO, Tcp}
import akka.util.ByteString
import akka.io.Tcp._
import akka.actor._

object Socket {
  def props(remote: InetSocketAddress, simul: ActorRef /*,bdActor:ActorRef*/ ) =
    Props(classOf[Socket], remote, simul: ActorRef /*,bdActor:ActorRef*/ )
}

case class SendMessage(message: BachTInstr)

case object Init

class Socket(remote: InetSocketAddress, simul: ActorRef /*,bdActor*/ ) extends Actor {

  import Tcp._
  import context.system

  println(s"simul = $simul")
  // on essaie de se connecter à un serveur existant via l'acteur "IO"
  IO(Tcp) ! Connect(remote)

  /**
    *  3 messages possible:
    *   - l'acteur "simul" qui permet d'exécuter le code BachT venant de l'autre machine
    *   - CommandFail si un serveur TCP n'existe pas déjà => à ce moment là, on devient un serveur TCP
    *   - Connected quand il arrive à se connecter à un serveur, il devient client
    */
  def receive = {
    case CommandFailed(_: Connect) => // si il n'arrive pas à s'y connecter, il devient serveur
      println("connect failed to server, becoming the server.")
      IO(Tcp) ! Bind(self, remote)

      var handler: Option[ActorRef] = None
      simul ! "A"
      context.become { // ce comportement, gère les connections.
        case Bound(local) =>
          println(s"Server started on $local")
        case Connected(remote, local) => // lorsqu'on recoit une nouvelle connection d'un client
          handler = Some(context.actorOf(Props(new TCPConnectionHandler(sender, simul))))
          println(s"New connnection: $local -> $remote")
          sender() ! Register(handler.get)
        case m @ SendMessage(message) => // ici, on transfère le message venant de la GUI à l'acteur TCPConnectionHandler qui s'occupe de la communication.
          handler match {
            case None        => println("No one connected yet.")
            case Some(actor) => actor ! m
          }
      }

    case c @ Connected(remote, local) => // si il arrive à se connecter à un serveur, il devient client.
      println("connected")
      simul ! "B"

      val connection = sender()
      connection ! Register(self)
      context.become {
        case SendMessage(message) =>
          println("Sending message: " + message)
          /*println(
            "supposed to receive: " + deballageDuCadeau(
              JacksonWrapper
                .deserialize[ChristmasGift](
                  ByteString(JacksonWrapper.serialize(messageSending(message))).utf8String
                )
            )
          )*/
          connection ! Write(ByteString(JacksonWrapper.serialize(messageSending(message))))
        //case data: ByteString =>
        //connection ! Write(data)
        case CommandFailed(w: Write) =>
        case Received(data) => // c'est ici que l'on récupère les données reçues. One peut les envoyer à l'acteur qui s'occupe de la BD.
          val decoded: BachTInstr =
            messageReception(JacksonWrapper.deserialize[MessageSend](data.utf8String))
          println(s"They told us: $decoded")
          simul ! decoded
        case "close" =>
          connection ! Close
        case _: ConnectionClosed =>
          context.stop(self)
      }
  }

  class TCPConnectionHandler(sender: ActorRef, simul: ActorRef) extends Actor { // s'occupe de la communication
    override def receive: Actor.Receive = {
      case Received(data) => // c'est ici que l'on récupère les données reçues. One peut les envoyer à l'acteur qui s'occupe de la BD.
        val decoded: BachTInstr = messageReception(
          JacksonWrapper.deserialize[MessageSend](data.utf8String)
        )
        println(s"They told us: $decoded")
        simul ! decoded
      case message: ConnectionClosed =>
        println("Connection has been closed")
        context stop self
      case SendMessage(message) =>
        println("Sending message: " + message)
        /*println(
          "supposed to receive: " + messageReception(
            JacksonWrapper
              .deserialize[MessageSend](
                ByteString(JacksonWrapper.serialize(messageSending(message))).utf8String
              )
          )
        )*/
        sender ! Write(ByteString(JacksonWrapper.serialize(messageSending(message))))
    }
  }

  def messageReception(cg: MessageSend): BachTInstr =
    cg match {
      case MessageSend("tell", token, data) =>
        Tell(token: String, data: Map[String, Data])
      case MessageSend("get", token, data) =>
        Get(token: String, data: Map[String, Data])
      case MessageSend("ask", token, data) =>
        Ask(token: String, data: Map[String, Data])
      case MessageSend("nask", token, data) =>
        Nask(token: String, data: Map[String, Data])
    }
  def messageSending(inst: BachTInstr): MessageSend =
    inst match {
      case Tell(token: String, data: Map[String, Data]) =>
        MessageSend("tell", token, data)
      case Get(token: String, data: Map[String, Data]) =>
        MessageSend("get", token, data)
      case Ask(token: String, data: Map[String, Data]) =>
        MessageSend("ask", token, data)
      case Nask(token: String, data: Map[String, Data]) =>
        MessageSend("nask", token, data)
    }
}
