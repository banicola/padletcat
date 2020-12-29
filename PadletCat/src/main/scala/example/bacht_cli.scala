/* --------------------------------------------------------------------------

   Complete code for the command-line interpreter


   AUTHOR : J.-M. Jacquet and D. Darquennes
   DATE   : March 2016

----------------------------------------------------------------------------*/

package example

class Expr
case class bacht_ast_empty_agent()                                  extends Expr
case class bacht_ast_primitive(primitive: String, token: String, title: String, content: String)    extends Expr
case class bacht_ast_agent(op: String, agenti: Expr, agentii: Expr) extends Expr
import scala.util.parsing.combinator._
import scala.util.matching.Regex
import akka.actor._
import java.net.InetSocketAddress

import akka.util.Timeout
import scala.concurrent.{Future}
import akka.pattern.{ask, pipe}
import java.util.concurrent.TimeUnit.SECONDS
import scala.concurrent.Await

class BachTParsers extends RegexParsers {

  def token: Parser[String] = ("[0-9a-zA-Z_]+").r ^^ { _.toString }
  def title : Parser[String] = ("^.+?(?=\\,)").r ^^ {_.toString}
  def content : Parser[String] = ("^.+?(?=\\))").r ^^ {_.toString}
  def separator : Parser[String] = (",").r ^^ {_.toString}

  val opChoice: Parser[String] = "+"
  val opPara: Parser[String]   = "||"
  val opSeq: Parser[String]    = ";"

  def primitive: Parser[Expr] =
    "tell(" ~ token ~ ")" ^^ {
      case _ ~ vtoken ~ _ => bacht_ast_primitive("tell", vtoken, "", "")
    } |
    "tell(" ~ token ~ separator ~ title ~ separator ~ content ~ ")" ^^ {
      case _ ~ vtoken ~ vseparator ~ vtitle ~ vseparator2 ~ vcontent ~ _ => bacht_ast_primitive("tell", vtoken, vtitle, vcontent)
    } |
    "ask(" ~ token ~ ")" ^^ {
      case _ ~ vtoken ~ _ => bacht_ast_primitive("ask", vtoken)
    } |
    "get(" ~ token ~ ")" ^^ {
      case _ ~ vtoken ~ vauthor ~ _ => bacht_ast_primitive("get", vtoken)
    } |
    "nask(" ~ token ~ ")" ^^ {
      case _ ~ vtoken ~ vauthor ~ _ => bacht_ast_primitive("nask", vtoken)
    }

  def agent = compositionChoice

  def compositionChoice: Parser[Expr] =
    compositionPara ~ rep(opChoice ~ compositionChoice) ^^ {
      case ag ~ List()           => ag
      case agi ~ List(op ~ agii) => bacht_ast_agent(op, agi, agii)
    }

  def compositionPara: Parser[Expr] =
    compositionSeq ~ rep(opPara ~ compositionPara) ^^ {
      case ag ~ List()           => ag
      case agi ~ List(op ~ agii) => bacht_ast_agent(op, agi, agii)
    }

  def compositionSeq: Parser[Expr] =
    simpleAgent ~ rep(opSeq ~ compositionSeq) ^^ {
      case ag ~ List()           => ag
      case agi ~ List(op ~ agii) => bacht_ast_agent(op, agi, agii)
    }

  def simpleAgent: Parser[Expr] = primitive | parenthesizedAgent

  def parenthesizedAgent: Parser[Expr] = "(" ~> agent <~ ")"

}

object BachTSimulParser extends BachTParsers {

  def parse_primitive(prim: String) = parseAll(primitive, prim) match {
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }

  def parse_agent(ag: String) = parseAll(agent, ag) match {
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }

}
import scala.swing._

case class MessageSend(instr: String, token: String)

import scala.util.Random
import language.postfixOps

/**
  * Cette classe est un acteur qui exécute le code écrit en BachT.
  *
  */
class BachTSimul(var bb: BachTStore) {

  // creating the socket actor in order to communicate with the other app.
  val socket = context.actorOf(
    Socket.props(new InetSocketAddress("localhost", 8080), self /*,bdActor*/ ),
    "TCPSocket"
  )

  var store: ActorRef  = _
  var username: String = _

  /**
    * Elle communique en recevant 2 messages.
    * Soit la commande à exécuter provenant de l'interface graphique de cette même app. Une réponse est envoyé au "sender" = l'acteur responsable de l'interface grpahique.
    * Soit une instruction provenant de l'autre app. L'autre app ne recoit aucun réponse sute à cette instruction.
    */
  def receive = {
    case name: String =>
      username = name
      store = context.actorOf(
        Props(new BachTStore(username)),
        "store"
      )
    case Init => sender ! username
    case Command(command, data) => // prend en "argument" la commende écrite en BachT et les données manipulées.
      val agent_parsed = BachTSimulParser.parse_agent(command)
      bacht_exec_all(agent_parsed, data)
    case i: BachTInstr =>
      println("simulator received: " + i)
      store ! i
    case a: ActorRef =>
      println("gui actor defined in simul")
      println(s"the store is $store")
      store ! a
  }

  val bacht_random_choice = new Random()

  // code du prof. J'ai uniquement rajouté "data" afin de faciliter l'utilisation de bachT.
  def run_one(agent: Expr): (Boolean, Expr) = {

    agent match {
      case bacht_ast_primitive(prim, token, title, content) => {
        if (exec_primitive(prim, token, title, content)) {
          (true, bacht_ast_empty_agent())
        } else {
          (false, agent)
        }
      }

      case bacht_ast_agent(";", ag_i, ag_ii) => {
        run_one(ag_i, data) match {
          case (false, _)                      => (false, agent)
          case (true, bacht_ast_empty_agent()) => (true, ag_ii)
          case (true, ag_cont)                 => (true, bacht_ast_agent(";", ag_cont, ag_ii))
        }
      }

      case bacht_ast_agent("||", ag_i, ag_ii) => {
        var branch_choice = bacht_random_choice.nextInt(2)
        if (branch_choice == 0) {
          run_one(ag_i) match {
            case (false, _) => {
              run_one(ag_ii) match {
                case (false, _)                      => (false, agent)
                case (true, bacht_ast_empty_agent()) => (true, ag_i)
                case (true, ag_cont) =>
                  (true, bacht_ast_agent("||", ag_i, ag_cont))
              }
            }
            case (true, bacht_ast_empty_agent()) => (true, ag_ii)
            case (true, ag_cont) =>
              (true, bacht_ast_agent("||", ag_cont, ag_ii))
          }
        } else {
          run_one(ag_ii) match {
            case (false, _) => {
              run_one(ag_i) match {
                case (false, _)                      => (false, agent)
                case (true, bacht_ast_empty_agent()) => (true, ag_ii)
                case (true, ag_cont) =>
                  (true, bacht_ast_agent("||", ag_cont, ag_ii))
              }
            }
            case (true, bacht_ast_empty_agent()) => (true, ag_i)
            case (true, ag_cont)                 => (true, bacht_ast_agent("||", ag_i, ag_cont))
          }
        }

      }

      case bacht_ast_agent("+", ag_i, ag_ii) => {
        var branch_choice = bacht_random_choice.nextInt(2)
        if (branch_choice == 0) {
          run_one(ag_i) match {
            case (false, _) => {
              run_one(ag_ii) match {
                case (false, _) => (false, agent)
                case (true, bacht_ast_empty_agent()) =>
                  (true, bacht_ast_empty_agent())
                case (true, ag_cont) => (true, ag_cont)
              }
            }
            case (true, bacht_ast_empty_agent()) =>
              (true, bacht_ast_empty_agent())
            case (true, ag_cont) => (true, ag_cont)
          }
        } else {
          run_one(ag_ii) match {
            case (false, _) => {
              run_one(ag_i) match {
                case (false, _) => (false, agent)
                case (true, bacht_ast_empty_agent()) =>
                  (true, bacht_ast_empty_agent())
                case (true, ag_cont) => (true, ag_cont)
              }
            }
            case (true, bacht_ast_empty_agent()) =>
              (true, bacht_ast_empty_agent())
            case (true, ag_cont) => (true, ag_cont)
          }
        }
      }
    }
  }

  // code du prof. J'ai uniquement rajouté "data" afin de faciliter l'utilisation de bachT.
  def bacht_exec_all(agent: Expr): Boolean = {
    var failure = false
    var c_agent = agent
    while (c_agent != bacht_ast_empty_agent() && !failure) {
      failure = run_one(c_agent) match {
        case (false, _) => true
        case (true, new_agent) => {
          c_agent = new_agent
          false
        }
      }
    }

    if (c_agent == bacht_ast_empty_agent()) {
      //println("Success\n")
      true
    } else {
      //println("failure\n")
      false
    }
  }

  def exec_primitive(prim: String, token: String , title: String, content: String): Boolean = {
    prim match {
      case "tell" if (title != "" && content != "")=> bb.tell(token, title, content)
      case "tell" => bb.tell(token)
      case "ask" => bb.ask(token)
      case "get" => bb.get(token)
      case "nask" => bb.nask(token)
    }
  }
}