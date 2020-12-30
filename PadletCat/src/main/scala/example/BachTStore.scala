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

  var gui: ActorRef = _
  var theStore = Map[String,Data]()

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
          println("tell")
          sender ! tell(token, data)
        case Ask(token: String, data: Map[String, Data]) =>
          println("ask")
          sender ! ask(token, data)
        case Get(token: String, data: Map[String, Data]) =>
          println("get")
          sender ! get(token, data)
        case Nask(token: String, data: Map[String, Data]) =>
          println("nask")
          sender ! nask(token, data)
      }
    case a: ActorRef =>
      println("gui actor defined")
      gui = a

      //Hardcodage d'un padlet---------
      /*
      var test_padlet = List("Chartreux")
      var test_data = Map("1" -> Data("A", "30/12/2020 03:37 PM",
      "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",
      "video", "Sleeping beauty", "Yuzu ^-^", test_padlet))
      theStore = theStore ++ test_data
      tell("1", test_data)
      */
      //-------------------------------

    case _ => println("received smth strange")

  }

  def tell(token: String, data: Map[String, Data]): Boolean = {

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
      if (theStore.contains(token)) {
        theStore.remove(token)
        gui ! RemoveFromGui(token)
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
    var text: String,
    var padlet: List[String]
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
      case e: Exception => e.printStackTpadlet
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
      case e: Exception => e.printStackTpadlet
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
      case e: Exception => e.printStackTpadlet
    }
    connection.close
    result
  }

  def nask(token: String, data: Map[String, Data]): Boolean = {
    // vérifier si l'élément n'est pas présent
    !ask(token, data)
  }
  */
