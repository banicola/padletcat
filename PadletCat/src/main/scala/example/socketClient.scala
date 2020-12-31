package example

import java.net.InetSocketAddress
import akka.io.{IO, Tcp}
import akka.util.ByteString
import akka.io.Tcp._
import akka.actor._
import scala.collection.mutable.Map

object Socket {
  def props(remote: InetSocketAddress, simul: ActorRef /*,bdActor:ActorRef*/ ) =
    Props(classOf[Socket], remote, simul: ActorRef /*,bdActor:ActorRef*/ )
}

case class SendMessage(message: BachTInstr)

case object Init

class Socket(remote: InetSocketAddress, simul: ActorRef) extends Actor {

  import Tcp._
  import context.system

  println(s"simul = $simul")
  IO(Tcp) ! Connect(remote)

  /**
    *  3 messages possible:
    *   - l'acteur "simul" qui permet d'exécuter le code BachT venant de l'autre machine
    *   - CommandFail si un serveur TCP n'existe pas déjà => à ce moment là, on devient un serveur TCP
    *   - Connected quand il arrive à se connecter à un serveur, il devient client
    */
  def receive = {
    case CommandFailed(_: Connect) =>
      println("connect failed to server, becoming the server.")
      IO(Tcp) ! Bind(self, remote)

      var handler: Option[ActorRef] = None
      simul ! "Amélie"
      context.become {
        case Bound(local) =>
          println(s"Server started on $local")
        case Connected(remote, local) =>
          handler = Some(context.actorOf(Props(new TCPConnectionHandler(sender, simul))))
          println(s"New connnection: $local -> $remote")
          sender() ! Register(handler.get)
        case m @ SendMessage(message) =>
          handler match {
            case None        => println("No one connected yet.")
            case Some(actor) => actor ! m
          }
      }

    case c @ Connected(remote, local) =>
      println("connected")
      simul ! "Bastien"

      val connection = sender()
      connection ! Register(self)
      context.become {
        case SendMessage(message) =>
          connection ! Write(ByteString(JacksonWrapper.serialize(messageSending(message))))
        case CommandFailed(w: Write) =>
        case Received(data) =>
          val decoded: BachTInstr =
            messageReception(JacksonWrapper.deserialize[MessageSend](data.utf8String))
          simul ! decoded
        case "close" =>
          connection ! Close
        case _: ConnectionClosed =>
          context.stop(self)
      }
  }

  class TCPConnectionHandler(sender: ActorRef, simul: ActorRef) extends Actor {
    override def receive: Actor.Receive = {
      case Received(data) =>
        val decoded: BachTInstr = messageReception(
          JacksonWrapper.deserialize[MessageSend](data.utf8String)
        )
        simul ! decoded
      case message: ConnectionClosed =>
        println("Connection has been closed")
        context stop self
      case SendMessage(message) => sender ! Write(ByteString(JacksonWrapper.serialize(messageSending(message))))        
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
