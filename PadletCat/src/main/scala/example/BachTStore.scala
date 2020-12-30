package example


import akka.actor._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.swing._

/* --------------------------------------------------------------------------

   The BachT store


   AUTHOR : J.-M. Jacquet and D. Darquennes
   DATE   : March 2016

----------------------------------------------------------------------------*/

class BachTStore(clientName: String) extends Actor {

  var theStore = Map[String,Data]()
  var gui: ActorRef = _

  /**
    *  On recoit soit des instruction classique provenant initalement de la GUI
    *  Soit des instruction "NoFeedback" provenant de l'autre app et ne demandant pas de réponse.
    *  Les reponses sont envoyées dans les fonctions, au client. (varibale client)
    *  Les réponses au "sender" sont destinées au BachTSimul afin de savoir si la commande c'est bien exécutée.
    */
  def receive = {
    case x: BachTInstr =>
      println("received a BachTInstr: "+x)
      x match {
        case Tell(token: String, data: Map[String, Data]) =>
          sender ! tell(token, data)
        case Ask(token: String, data: Map[String, Data]) =>
          sender ! ask(token, data)
        case Get(token: String, data: Map[String, Data]) =>
          sender ! get(token, data)
        case Nask(token: String, data: Map[String, Data]) =>
          sender ! nask(token, data)
      }
    case a: ActorRef =>
      println("gui actor defined")
      gui = a
    case _ => println("received smth strange")

  }

  def tell(token: String, data: Map[String, Data]): Boolean = {

    println("Enter the tell")

    val lesDatas        = data(token)
    var result: Boolean = false

    if (!theStore.contains(token)) {
      theStore = theStore ++ Map(token -> lesDatas)
      result = true
    }

    gui ! AddToGui(token, lesDatas)

    result
  }

  def isEmpty():Boolean = theStore.isEmpty

  def ask(token:String, data: Map[String, Data]):Boolean = {
    theStore.contains(token)
  }

  def get(token:String, data: Map[String, Data]):Boolean = {
    if(token=="All"){
      data foreach {case (key, value) => gui ! AddToGui(key, value)}
      true
    } else {
      if (theStore.contains(token) && theStore.get(token).get.author == data.get(token).get.author) {
        theStore.remove(token)
        gui ! RemoveFromGui(token, data.get(token).get)
        true
      }
      else {
        false
        }
    }

  }

  def nask(token:String, data: Map[String, Data]):Boolean = {
    !theStore.contains(token)
  }

  def clear_store {
    theStore = Map[String,Data]()
  }

}

case class Data(
    var author: String,
    var date: String,
    var image: String,
    var video: String,
    var title: String,
    var description: String,
    var race: List[String]
)



























/*
  // chaque utilisateur a une DB.
  var dbname = if (clientName == "A") {
    "bacht"
  } else {
    "bacht2"
  }
  println("my db is :" + dbname)

  val url                    = s"jdbc:sqlite:$dbname.db"
  val driver                 = "com.mysql.jdbc.Driver"
  val username               = "root"
  val password               = ""
  var connection: Connection = _

  var gui: ActorRef = _

  /**
    *  On recoit soit des instruction classique provenant initalement de la GUI
    *  Soit des instruction "NoFeedback" provenant de l'autre app et ne demandant pas de réponse.
    *  Les reponses sont envoyées dans les fonctions, au client. (varibale client)
    *  Les réponses au "sender" sont destinées au BachTSimul afin de savoir si la commande c'est bien exécutée.
    */
  def receive = {
    case x: BachTInstr =>
      println("db received a BachTInstr: "+x)
      x match {
        case Tell(token: String, data: Map[String, Data]) =>
          sender ! tell(token, data)
        case Ask(token: String, data: Map[String, Data]) =>
          sender ! ask(token, data)
        case Get(token: String, data: Map[String, Data]) =>
          sender ! get(token, data)
        case Nask(token: String, data: Map[String, Data]) =>
          sender ! nask(token, data)
      }
    case a: ActorRef =>
      println("gui actor defined in db")
      gui = a
    case _ => println("db received smth strange")

  }

  def tell(token: String, data: Map[String, Data]): Boolean = {
    // ajout en DB

    val lesDatas        = data(token)
    var result: Boolean = false
    try {
      Class.forName(driver)
      connection = DriverManager.getConnection(url, username, password)

      var statement = connection.prepareStatement("SELECT MAX(id) FROM content WHERE owner = ?")
      statement.setString(1, lesDatas.owner)
      val rs = statement.executeQuery()

      val id: String = if (rs.next()) {
        if (rs.getString("MAX(id)") != null) {
          lesDatas.owner + (1 + rs.getString("MAX(id)").replaceAll("[^0-9]", "").toInt).toString()
        } else {
          lesDatas.owner + 0.toString()
        }
      } else {
        lesDatas.owner + 0.toString()
      }

      statement = connection.prepareStatement(
        "INSERT INTO content (id, owner,image, video, description, title,date) VALUES (?, ?, ?, ?, ?,?,?)"
      )
      statement.setString(1, id)
      statement.setString(3, lesDatas.owner)
      statement.setString(4, lesDatas.image)
      statement.setString(5, lesDatas.video)
      statement.setString(6, lesDatas.description)
      statement.setString(7, lesDatas.title)
      statement.setString(8, lesDatas.date)
      statement.executeUpdate()

      gui ! AddToGui(id, lesDatas)

      result = true
    } catch {
      case e: Exception => e.printStackTrace
    }
    connection.close
    result
  }

  def ask(token: String, data: Map[String, Data]): Boolean = {
    // vérifier si l'élément est présent
    var result: Boolean = false
    try {
      Class.forName(driver)
      connection = DriverManager.getConnection(url, username, password)
      val statement = connection.prepareStatement("SELECT COUNT(1) FROM content WHERE id = ?")
      statement.setString(1, token)
      val rs = statement.executeQuery()
      rs.next()
      if (rs.getInt("COUNT(1)") == 1) {
        result = true
      }
    } catch {
      case e: Exception => e.printStackTrace
    }
    connection.close
    result
  }

  def get(token: String, data: Map[String, Data]): Boolean = {
    // récuperer l'élément
    var result: Boolean = false
    try {
      Class.forName(driver)
      connection = DriverManager.getConnection(url, username, password)
      val statement = connection.createStatement

      if (token == "All") {
        val rs = statement.executeQuery("SELECT * FROM content")

        while (rs.next()) {

          val id       = rs.getString("id")
          val owner    = rs.getString("owner")
          val date     = rs.getString("date")
          val title    = rs.getString("title")
          val description     = rs.getString("description")
          val image    = rs.getString("image")
          val video    = rs.getString("video")

          gui ! AddToGui(id, Data(owner, date, image, video, title, description, null))
        }
      } else {
        var statement = connection.prepareStatement(
          "SELECT * FROM content c WHERE c.id=?"
        )
        statement.setString(1, token)
        var rs = statement.executeQuery()
        rs.next()

        statement = connection.prepareStatement(
          "SELECT * FROM tag t WHERE t.contentId = ?"
        )
        statement.setString(1, token)
        var rsTag = statement.executeQuery()

        val id       = rs.getString("id")
        val owner    = rs.getString("owner")
        val date     = rs.getString("date")
        val title    = rs.getString("title")
        val description     = rs.getString("description")
        val image    = rs.getString("image")
        val video    = rs.getString("video")

        gui ! RemoveFromGui(id, Data(owner, date, image, video, title, description, null))
        statement = connection.prepareStatement("DELETE FROM content WHERE id=?")
        statement.setString(1, token)
        statement.executeUpdate()
      }
      result = true
    } catch {
      case e: Exception => e.printStackTrace
    }
    connection.close
    result
  }

  def nask(token: String, data: Map[String, Data]): Boolean = {
    // vérifier si l'élément n'est pas présent
    !ask(token, data)
  }
  */
