package example

import scalafx.Includes._
import scalafx.application.{Platform, JFXApp}
import scalafx.application.JFXApp.PrimaryStage
import scalafx.geometry.Pos
import scalafx.scene.Scene
import scalafx.scene.paint.Color._
import scalafx.scene.shape._
import scalafx.scene.text._
import scalafx.scene.layout._
import scalafx.scene.control.TextField
import scalafx.scene.control.Button
import scalafx.scene.control.ToggleButton
import scalafx.scene.control.ToggleGroup
import scalafx.scene.control.ComboBox
import scalafx.scene.control.TextArea
import scalafx.scene.control.Label
import scalafx.scene.control.ScrollPane
import scalafx.scene.image._
import scalafx.stage.FileChooser
import scalafx.stage.FileChooser.ExtensionFilter
import javafx.scene.input.MouseEvent
import javafx.geometry.Insets
import javafx.event.EventHandler
import javafx.event.ActionEvent
import javafx.stage.WindowEvent
import java.text.SimpleDateFormat
import java.util.{Calendar, Date, Base64}
import java.io.File
import java.io.ByteArrayInputStream
import java.nio.file.Files
import akka.actor._
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._
import akka.util.Timeout
import scala.util.Random
import scala.concurrent.{Future}
import akka.pattern.{ask, pipe}
import java.util.concurrent.TimeUnit.SECONDS
import scala.concurrent.Await

object Main extends JFXApp {
  println("Starting")

  private implicit val timeout = Timeout(15, SECONDS)

  val system = ActorSystem("MyActorSys")

  // creating the bacht simulator Actor.
  val simul = system.actorOf(
    Props(new BachTSimul()),
    "simul"
  )

  val dbActor = system.actorOf(
    Props(new DBActor(simul)),
    "dbActor"
  )

  Thread.sleep(5000)

  val response = simul ? Init

  val username = Await.result(response, timeout.duration).asInstanceOf[String]

  println("gui actor definition sent to simul")
  simul ! dbActor

  val grilleContenu = new FlowPane(20, 20) {
    style = "-fx-background-color:transparent"
  }

  case class Filter(val name: String, val location: String, val tag: String)

  var datas             = ListBuffer[(String, Data)]()
  var filterButtonsList = List[ToggleButton]()
  var filters: ListBuffer[Filter] = ListBuffer(
    Filter("Visites - Canada", "Canada", "Visites"),
    Filter("UNamur weekend", "Belgique", "Soirées"),
    Filter("Mémoire à Singapour", "Singapour", "Mémoire")
  )
  var locationsName: List[String] = List("Canada", "Singapour", "Belgique")
  var tagsName: List[String]      = List("Soirées", "Mémoire", "Visites")
  val toggleGroup: ToggleGroup    = new ToggleGroup();

  dbActor ! Load

  stage = new PrimaryStage {
    title = "Erasm'Share"
    width = 1920
    height = 960

    val mainScene: Scene = new Scene {

      def createFiltersButton(): List[ToggleButton] = {
        for (filter <- filters.toList.map(_.name)) yield {
          new ToggleButton(filter) {
            userData = filter
            padding = new Insets(10)
            alignment = Pos.CenterLeft
            margin = new Insets(20)
            onAction = new EventHandler[ActionEvent] {
              override def handle(event: ActionEvent) {
                setGui(datas)
              }
            }
          }
        }
      }

      def deleteFiltersButton(): Unit = {
        for (button <- filterButtonsList) yield {
          if (button.isSelected) {
            filters -= filters.filter(_.name == button.getText).head
          }
        }
      }

      stylesheets += getClass.getResource("style.css").toExternalForm
      //background color
      fill = rgb(21, 21, 21)
      var selectedImage: File = null

      val mainStack = new StackPane

      val addButton     = new Image("add.png", 90, 90, true, true)
      val addButtonView = new ImageView(addButton)
      addButtonView.onMouseClicked = new EventHandler[MouseEvent] {
        override def handle(event: MouseEvent) {
          mainStack.children += ajoutContenu
        }
      }
      addButtonView.margin = new Insets(0, 60, 220, 0)

      // HEADER BEGIN -----------------------------------

      val logo     = new Image("erasmshare.png")
      val logoView = new ImageView(logo)

      val logoHBox = new HBox(20)
      val style =
        "-fx-background-color: #EA6939; -fx-text-fill: #FFFFFF; -fx-font-weight: bold; -fx-font-family: Helvetica Neue; -fx-font-size: 18px;"
      logoHBox.setStyle(style)
      logoHBox.setPadding(new Insets(10, 0, 0, 0))

      logoHBox.children = List(logoView)

      val profileImg  = new Image("profile.png")
      val profileView = new ImageView(profileImg)

      val profile = new Button(username) {
        graphic = profileView
        style =
          "-fx-background-color: #EA6939; -fx-text-fill: #FFFFFF; -fx-font-weight: bold; -fx-font-family: Helvetica Neue; -fx-font-size: 18px;"
      }

      profile.margin_=(new Insets(20))
      profile.setAlignment(Pos.CenterRight)

      val headerHBox = new BorderPane {
        style = "-fx-background-color: rgb(234,105,57); "
      }
      //headerHBox.setPrefHeight(70)
      headerHBox.setPadding(new Insets(10, 20, 10, 40))
      headerHBox.left = logoHBox
      headerHBox.right = profile

      // HEADER END -----------------------------------

      // Content begin ---------------------------------------------------------------

      val styleFilterSelected =
        "-fx-background-color: #ea6939; -fx-text-fill: #FFFFFF; -fx-font-family: Helvetica Neue; -fx-font-size: 18px; -fx-background-radius: 5 5 5 5"
      val styleFilterHovered =
        "-fx-background-color: rgba(234,105,57,0.3); -fx-border-color: rgba(255,255,255,0); -fx-text-fill: #ea6939; -fx-font-family: Helvetica Neue; -fx-font-size: 18px; -fx-background-radius: 5 5 5 5"
      val styleFilterNotSelected =
        "-fx-background-color: rgba(255,255,255,0); -fx-border-color: rgba(255,255,255,0); -fx-text-fill: #ea6939; -fx-font-family: Helvetica Neue; -fx-font-size: 18px; -fx-background-radius: 5 5 5 5"

      //display filters from list given
      /*for (filter <- filtersName.toList) {
        val filterButton = new ToggleButton(filter){
          padding = new Insets(10)
          alignment = Pos.CenterLeft
        }
        filterButton.margin_=(new Insets(20))

        filterButtonsList += filterButton
      }*/

      filterButtonsList = createFiltersButton()

      filterButtonsList.map(_.setToggleGroup(toggleGroup))

      val editImg     = new Image("edit.png")
      val editImgView = new ImageView(editImg)

      val editFilters = new Button {
        graphic = editImgView
        style =
          "-fx-background-color: rgba(255,255,255,0); -fx-border-color: rgba(255,255,255,0); -fx-text-fill: #ea6939; -fx-font-family: Helvetica Neue; -fx-font-size: 18px; -fx-background-radius: 5 5 5 5"
        padding = new Insets(10)
      }
      editFilters.setAlignment(Pos.CenterRight)

      // ***************** pannel edit filters BEGIN **********************************

      val addImg     = new Image("addFilter.png")
      val addImgView = new ImageView(addImg)

      val deleteImg     = new Image("delete.png")
      val deleteImgView = new ImageView(deleteImg)

      val cancelImg     = new Image("cancel.png")
      val cancelImgView = new ImageView(cancelImg)

      val deleteEditFilters = new Button {
        graphic = deleteImgView
        style =
          "-fx-background-color: rgba(255,255,255,0); -fx-border-color: rgba(255,255,255,0); -fx-text-fill: #ea6939; -fx-font-family: Helvetica Neue; -fx-font-size: 18px; -fx-background-radius: 5 5 5 5"
        padding = new Insets(10)

      }

      val cancelEditFilters = new Button {
        graphic = cancelImgView
        style =
          "-fx-background-color: rgba(255,255,255,0); -fx-border-color: rgba(255,255,255,0); -fx-text-fill: #ea6939; -fx-font-family: Helvetica Neue; -fx-font-size: 18px; -fx-background-radius: 5 5 5 5"
        padding = new Insets(10)
      }

      val addFilter = new Button {
        graphic = addImgView
        style =
          "-fx-background-color: rgba(255,255,255,0); -fx-border-color: rgba(255,255,255,0); -fx-text-fill: #ea6939; -fx-font-family: Helvetica Neue; -fx-font-size: 18px; -fx-background-radius: 5 5 5 5"
        padding = new Insets(10)
      }

      // ################################## FILTERS POP UP BEGIN ##################################

      val filterPanesStyle =
        "-fx-background-color: rgb(255,255,255); -fx-background-radius: 10 10 10 10"

      val filterBoxTitle = new Label("Créer un filtre") {
        style =
          "-fx-background-color: rgb(255,255,255); -fx-text-fill: #ea6939; -fx-font-family: Helvetica Neue; -fx-font-size: 22px;"
      }

      val closeImage     = new Image("close.jpg")
      val closeImageView = new ImageView(closeImage)
      closeImageView.onMouseClicked = new EventHandler[MouseEvent] {
        override def handle(event: MouseEvent) {
          println("close")
          mainStack.children.remove(addStackFilter)
        }
      }

      val orangeLineFilter = new Line {
        stroke = rgb(234, 105, 57)
        strokeWidth = 2
        startX = 31
        endX = 930
      }

      val addFilterTopPane = new BorderPane
      addFilterTopPane.setStyle("-fx-background-color: rgb(255,255,255)")
      addFilterTopPane.setPadding(new Insets(0, 0, 15, 0))

      addFilterTopPane.left = filterBoxTitle
      addFilterTopPane.right = closeImageView
      addFilterTopPane.bottom = orangeLineFilter

      val addFilterName = new TextField {
        minWidth = 400
        maxWidth = 500
      }

      val labelTitre = new Label("Entrez le titre")

      val vboxTitre = new VBox
      vboxTitre.children = List(labelTitre, addFilterName)

      val localisation = new ComboBox(locationsName) {
        minWidth = 185
        maxWidth = 185
      }

      val labelLocalisation = new Label("Localisation")
      val vboxLocalisation  = new VBox
      vboxLocalisation.children = List(labelLocalisation, localisation)

      val tag = new ComboBox(tagsName) {
        minWidth = 185
        maxWidth = 185
      }
      val labelTag = new Label("Tags")
      val vboxTag  = new VBox
      vboxTag.children = List(labelTag, tag)

      val contentAddFilterPane = new HBox(20)
      contentAddFilterPane.setStyle("-fx-background-color: rgb(255,255,255);")
      contentAddFilterPane.setPadding(new Insets(0, 30, 0, 30))
      contentAddFilterPane.children = List(vboxTitre, vboxLocalisation, vboxTag)

      val greyLineFilter = new Line {
        stroke = rgb(150, 150, 150)
        strokeWidth = 1
        startX = 31
        endX = 930
      }

      val createButton = new Button("Créer") {
        style =
          "-fx-background-color: #EA6939; -fx-text-fill: #FFFFFF; -fx-font-weight: regular; -fx-font-family: Helvetica Neue; -fx-font-size: 16px; -fx-border-color: #ea6939; -fx-background-radius: 5 5 5 5"
        minWidth = 120
        maxWidth = 120
        minHeight = 30
        maxHeight = 35
      }

      val cancelButton = new Button("Annuler") {
        style =
          "-fx-background-color: #FFFFFF; -fx-text-fill: #EA6939; -fx-font-weight: regular; -fx-font-family: Helvetica Neue; -fx-font-size: 16px; -fx-border-color: #EA6939; -fx-background-radius: 5 5 5 5"
        minWidth = 120
        maxWidth = 120
        minHeight = 30
        maxHeight = 35
      }

      cancelButton.onMouseClicked = new EventHandler[MouseEvent] {
        override def handle(event: MouseEvent) {
          println("cancel add filter")
          mainStack.children.remove(addStackFilter)
        }
      }

      val bottomHBox = new HBox(20)
      //bottomHBox.setAlignment(Pos.BottomRight)
      bottomHBox.setStyle("-fx-background-color: #FFFFFF;")
      bottomHBox.setPadding(new Insets(10, 0, 0, 0))
      bottomHBox.children = List(cancelButton, createButton)

      val addFilterBottomPane = new BorderPane
      addFilterBottomPane.setStyle("-fx-background-color: rgb(255,255,255)")
      addFilterBottomPane.setPadding(new Insets(20))

      addFilterBottomPane.top = greyLineFilter
      addFilterBottomPane.right = bottomHBox

      val addFilterPane = new BorderPane
      addFilterPane.setPrefWidth(935)
      addFilterPane.setPrefHeight(253)
      addFilterPane.setPadding(new Insets(150, 150, 150, 150))

      addFilterPane.top = addFilterTopPane
      addFilterPane.center = contentAddFilterPane
      addFilterPane.bottom = addFilterBottomPane

      val addStackFilter = new StackPane {
        minWidth = 940
        minHeight = 253
        maxWidth = 950
        maxHeight = 253
        style = "-fx-background-color: rgb(255,255,255); -fx-background-radius: 15 15 15 15"
      }
      addStackFilter.setPadding(new Insets(0, 50, 0, 50))
      addStackFilter.children = List(addFilterPane)

      val popUpFilter = new BorderPane {
        minWidth = 1025
        minHeight = 253
        maxWidth = 1025
        maxHeight = 293
        style = "-fx-background-color: rgb(255,255,255); -fx-background-radius: 15 15 15 15"
      }
      //popUpFilter.setPadding(new Insets(0, 100, 0, 100))
      popUpFilter.center = addStackFilter

      createButton.onMouseClicked = new EventHandler[MouseEvent] {
        override def handle(event: MouseEvent) {
          println("create add filter")
          if (addFilterName.text.value != "" && localisation.value.value != null && tag.value.value != null) {
            filters += Filter(
              addFilterName.text.value,
              localisation.value.value,
              tag.value.value
            )
            filterButtonsList = createFiltersButton()
            filterButtonsList.map(_.setToggleGroup(toggleGroup))
            filtersHBox.children = filterButtonsList

            mainStack.children.remove(addStackFilter)
          }
        }
      }

      addFilter.onMouseClicked = new EventHandler[MouseEvent] {
        override def handle(event: MouseEvent) {
          mainStack.children += addStackFilter
        }
      }
      // #################################  FILTERS POP UP END  ##################################

      val filtersEditionHBox = new HBox(10)
      filtersEditionHBox.setAlignment(Pos.CenterRight)
      filtersEditionHBox.children = List(addFilter, deleteEditFilters, cancelEditFilters)
      filtersEditionHBox.setPadding(new Insets(0, 15, 0, 0))

      val styleFiltersPane =
        "-fx-background-color: rgb(255,255,255); -fx-background-radius: 5 5 5 5"

      val filtersEditionPane = new BorderPane
      filtersEditionPane.setStyle(styleFiltersPane)
      filtersEditionPane.setPrefWidth(1869)
      filtersEditionPane.left = filtersHBox
      filtersEditionPane.right = filtersEditionHBox

      // ***************** pannel edit filters END ************************************

      val filtersEditHBox = new HBox(10)
      filtersEditHBox.setAlignment(Pos.CenterRight)
      filtersEditHBox.children = List(editFilters)
      filtersEditHBox.setPadding(new Insets(0, 15, 0, 0))

      val filtersHBox = new HBox(10)
      filtersHBox.setAlignment(Pos.CenterLeft)
      filtersHBox.children = filterButtonsList.toList

      val filtersPane = new BorderPane
      filtersPane.setStyle(styleFiltersPane)

      filtersPane.left = filtersHBox
      filtersPane.right = filtersEditHBox

      editFilters.onAction = new EventHandler[ActionEvent] {
        override def handle(event: ActionEvent) {
          //changer l'affichage pour édition
          filtersPane.right = filtersEditionHBox
        }
      }

      cancelEditFilters.onAction = new EventHandler[ActionEvent] {
        override def handle(event: ActionEvent) {
          filtersPane.right = filtersEditHBox
        }
      }

      deleteEditFilters.onAction = new EventHandler[ActionEvent] {
        override def handle(event: ActionEvent) {
          //supprimer les filtres
          deleteFiltersButton()
          filterButtonsList = createFiltersButton()
          filterButtonsList.map(_.setToggleGroup(toggleGroup))
          filtersHBox.children = filterButtonsList

        }
      }

      val contentVBox = new VBox(20)
      contentVBox.children = List(filtersPane)

      contentVBox.setPrefWidth(1920)
      contentVBox.setPrefHeight(1000)
      contentVBox.setPadding(new Insets(30, 40, 30, 40))
      contentVBox.setStyle(
        "-fx-background-color: rgba(255,255,255); -fx-background-image: url(\"background2.png\"); -fx-background-size: 1920, 1080; -fx-background-repeat: no-repeat;"
      )

      // Content end ------------------------------------------------------------------

      val homePane = new BorderPane
      homePane.top = headerHBox
      homePane.center = contentVBox

      val grilleContenuScrollPane = new ScrollPane {
        fitToWidth = true
        style = "-fx-background-color:transparent; -fx-background:transparent;"
        maxHeight = 700
      }
      grilleContenuScrollPane.content = grilleContenu

      contentVBox.children += grilleContenuScrollPane

      StackPane.setAlignment(addButtonView, Pos.BottomRight);

      mainStack.children = List(homePane, addButtonView)

      content = mainStack

      val ajoutContenu: BorderPane = {
        val rectangle = new Rectangle {
          width = 1560
          height = 600
          arcWidth = 30.0
          arcHeight = 30.0
          fill = rgb(255, 255, 255)
        }

        val orangeLine = new Line {
          stroke = rgb(234, 105, 57)
          strokeWidth = 2
          startX = 0
          endX = 1520
        }

        val greyLine = new Line {
          stroke = rgb(150, 150, 150)
          strokeWidth = 1
          startX = 0
          endX = 1520
        }

        val closeImage     = new Image("close.jpg")
        val closeImageView = new ImageView(closeImage)

        closeImageView.onMouseClicked = new EventHandler[MouseEvent] {
          override def handle(event: MouseEvent) {
            println("close")
            mainStack.children.remove(ajoutContenu)
          }
        }

        val addLocalisation     = new Image("add.jpg")
        val addLocalisationView = new ImageView(addLocalisation)
        addLocalisationView.onMouseClicked = new EventHandler[MouseEvent] {
          override def handle(event: MouseEvent) {
            println("add localisation")
          }
        }

        val addFilter     = new Image("add.jpg")
        val addFilterView = new ImageView(addFilter)
        addFilterView.onMouseClicked = new EventHandler[MouseEvent] {
          override def handle(event: MouseEvent) {
            println("add Filter")
          }
        }

        val upload     = new Image("upload.png")
        val uploadView = new ImageView(upload)

        val addContent = new VBox(20)

        val topBar = new BorderPane
        val ajoutTitle = new Text {
          text = "Proposer un nouveau contenu"
          style = "-fx-font-size: 22pt"
          fill = rgb(234, 105, 57)
        }

        topBar.left = ajoutTitle
        topBar.right = closeImageView

        val values = new HBox(20) {
          alignment = Pos.BottomLeft
        }

        val titre      = new TextField { minWidth = 960 }
        val labelTitre = new Label("Entrez le titre")
        val vboxTitre  = new VBox
        vboxTitre.children = List(labelTitre, titre)

        val localisation = new ComboBox(locationsName) {
          minWidth = 200
          maxWidth = 200
        }
        val labelLocalisation = new Label("Localisation")
        val vboxLocalisation  = new VBox
        vboxLocalisation.children = List(labelLocalisation, localisation)

        val tag = new ComboBox(tagsName) {
          minWidth = 200
          maxWidth = 200
        }
        val labelTag = new Label("Tags")
        val vboxTag  = new VBox
        vboxTag.children = List(labelTag, tag)

        values.children =
          List(vboxTitre, vboxLocalisation, addLocalisationView, vboxTag, addFilterView)

        val description = new TextArea {
          wrapText = true
        }

        val video      = new TextField
        val labelVideo = new Label("Lien de vidéo")
        val vboxVideo  = new VBox
        vboxVideo.children = List(labelVideo, video)

        val imageChooser = new FileChooser {
          title = "Choisissez une image"
          extensionFilters ++= Seq(
            new ExtensionFilter("Image Files", Seq("*.png", "*.jpg", "*.gif"))
          )
        }
        val addImageLabel = new Label("")
        val addImageButton = new Button("Ajouter une image") {
          graphic = uploadView
          style =
            "-fx-background-color: #ffffff; -fx-border-color: #ea6939; -fx-text-fill: #ea6939;"
        }
        addImageButton.onAction = new EventHandler[ActionEvent] {
          override def handle(event: ActionEvent) {
            selectedImage = imageChooser.showOpenDialog(stage)
            if (selectedImage != null) {
              addImageLabel.text = selectedImage.toString
            }
          }
        }

        val hboxImageSelector = new HBox(20) {
          alignment = Pos.CenterLeft
        }
        hboxImageSelector.children = List(addImageButton, addImageLabel)

        val bottomButtons = new HBox(20)
        val cancelButton = new Button("Annuler") {
          style =
            "-fx-background-color: #ffffff; -fx-border-color: #ea6939; -fx-text-fill: #ea6939;"
          cancelButton = true
        }
        cancelButton.onAction = new EventHandler[ActionEvent] {
          override def handle(event: ActionEvent) {
            mainStack.children.remove(ajoutContenu)
          }
        }
        val publishButton = new Button("Publier") {
          style =
            "-fx-background-color: #ea6939; -fx-border-color: #ea6939; -fx-text-fill: #ffffff;"
        }
        publishButton.onAction = new EventHandler[ActionEvent] {
          override def handle(event: ActionEvent) {
            val dateFormatter        = new SimpleDateFormat("dd/MM/yyyy hh:mm aa")
            var submittedDateConvert = new Date()
            val date                 = dateFormatter.format(submittedDateConvert)
            var encodedfile: String  = ""

            if (titre.text.value != "" && description.text.value != "" && localisation.value.value != null && tag.value.value != null) {
              if (selectedImage != null) {
                //transform file to string

                encodedfile =
                  Base64.getEncoder().encodeToString(Files.readAllBytes(selectedImage.toPath()))

              }

              dbActor ! Add(
                username,
                date,
                encodedfile,
                video.text.value,
                titre.text.value,
                description.text.value,
                localisation.value.value,
                List(tag.value.value)
              )

              mainStack.children.remove(ajoutContenu)

            } else {
              println("empty field")
            }

          }
        }
        bottomButtons.children = List(cancelButton, publishButton)

        val bottomBar = new BorderPane
        bottomBar.right = bottomButtons

        addContent.children = List(
          topBar,
          orangeLine,
          values,
          new Label("Composez le texte"),
          description,
          vboxVideo,
          hboxImageSelector,
          greyLine,
          bottomBar
        )
        addContent.setPadding(new Insets(20))

        val addStack = new StackPane {
          minWidth = 1560
          minHeight = 600
          maxWidth = 1560
          maxHeight = 600
        }
        addStack.children = List(rectangle, addContent)

        val pane = new BorderPane {
          minWidth = 1920
          minHeight = 960
          maxWidth = 1920
          maxHeight = 960
        }
        pane.center = addStack

        pane
      }
    }
    scene = mainScene
  }

  def addToGui(id: String, data: Data): Unit = {
    datas += ((id, data))

    setGui(datas)
  }

  def removeFromGui(id: String): Unit = {
    for (content <- datas) {
      if (content._1 == id) {
        datas -= content
      }
    }

    setGui(datas)
  }

  def setGui(datas: ListBuffer[(String, Data)]): Unit = {
    grilleContenu.children = List()

    val filteredDatas = if (toggleGroup.getSelectedToggle() != null) {
      val filter =
        filters.filter { x =>
          x.name == toggleGroup.getSelectedToggle.getUserData().toString()
        }.head
      datas.toList.filter { x =>
        x._2.location == filter.location && x._2.tags.contains(filter.tag)
      }
    } else {
      datas.toList
    }

    for ((id, data) <- filteredDatas.reverse) {
      grilleContenu.children += contentCell(
        id,
        data.owner,
        data.date,
        data.title,
        data.description,
        data.image,
        data.video
      )
    }
  }

  def contentCell(
      contentID: String,
      owner: String,
      date: String,
      title: String,
      text: String,
      image: String,
      video: String
  ): StackPane = {

    val deleteButton     = new Image("delete.png", 20, 20, true, true)
    val deleteButtonView = new ImageView(deleteButton)
    deleteButtonView.onMouseClicked = new EventHandler[MouseEvent] {
      override def handle(event: MouseEvent) {
        if (username == owner) {
          dbActor ! Remove(contentID)
        } else {
          println("L'utilisateur n'a pas la permission de supprimer ce contenu")
        }
      }
    }

    val deleteHBox = new BorderPane
    deleteHBox.right = deleteButtonView

    val userLabel = new Text {
      text = owner
      fill = rgb(72, 72, 72)
      font = Font("Helvetica", FontPosture.Italic, 14d)
    }
    val dateLabel = new Text {
      text = date
      fill = rgb(72, 72, 72)
      font = Font("Helvetica", FontPosture.Italic, 14d)
    }

    val titleLabel = new Text {
      text = title
      fill = rgb(234, 105, 57)
      style = "-fx-font-size: 22pt"
      wrappingWidth = 500
    }

    val texte = new Text(text) {
      fill = rgb(72, 72, 72)
      style = "-fx-font-size: 14pt"
      wrappingWidth = 500
    }

    val topBar = new BorderPane
    topBar.left = userLabel
    topBar.right = dateLabel

    val contentPane = new VBox(10) {
      maxWidth = 500
    }
    if (username == owner) {
      contentPane.children += deleteHBox
    }
    contentPane.children += topBar
    contentPane.children += titleLabel
    contentPane.children += texte
    contentPane.setPadding(new Insets(20))

    if (image != "") {
      val imageBytes       = Base64.getDecoder().decode(image)
      val contentImage     = new Image(new ByteArrayInputStream(imageBytes), 500, 500, true, true)
      val contentImageView = new ImageView(contentImage)
      contentPane.children += contentImageView
    }

    val rectangle = new Rectangle {
      width <== contentPane.width + 10
      height <== contentPane.height + 10
      arcWidth = 10.0
      arcHeight = 10.0
      fill = rgb(255, 255, 255)
    }

    val stack = new StackPane {
      maxWidth <== rectangle.width
      maxHeight <== rectangle.height
      id = contentID
    }
    stack.children = List(rectangle, contentPane)
    StackPane.setAlignment(rectangle, Pos.TopLeft);

    stack
  }

  stage.onCloseRequest = new EventHandler[WindowEvent] {
    override def handle(event: WindowEvent) {
      println("closing window")
      system.terminate()
    }
  }
}

class DBActor(simul: ActorRef) extends Actor {

  def addContent(
      owner: String,
      date: String,
      image: String,
      video: String,
      title: String,
      text: String,
      location: String,
      tags: List[String]
  ) = {
    val content: Data = Data(
      owner,
      date,
      image,
      video,
      title,
      text,
      location,
      tags
    )
    val x = Random.nextInt(Integer.MAX_VALUE)
    println(x.toString)
    simul ! Command(s"tell($x)", Map(x.toString -> content))
  }

  def receive = {
    case Add(
        owner: String,
        date: String,
        image: String,
        video: String,
        title: String,
        text: String,
        location: String,
        tags: List[String]
        ) =>
      addContent(owner, date, image, video, title, text, location, tags)
    case Remove(id: String) => simul ! Command(s"get($id)", Map())
    case Load               => simul ! Command("get(All)", Map())
    case x: Data            =>
      //println(s"le get a renvoyé $x et l'a supprimé")
      println(s"le get a supprimé l'élément $x")
    case x: String =>
      println(s"Le tell s'est bien effectué et a ajouté un objet avec l'id: $x")
    case AddToGui(id: String, data: Data) =>
      // ici, on peut j'ajouter à la GUI si il est dans les bonnes catégorie (cfr: filtres)
      Platform.runLater(
        Main.addToGui(id, data)
      )
    case RemoveFromGui(id: String, data: Data) =>
      Platform.runLater(
        Main.removeFromGui(id)
      )
    case _ => println("received unknown message")
  }
}

sealed trait Message
case class Add(
    owner: String,
    date: String,
    image: String,
    video: String,
    title: String,
    text: String,
    location: String,
    tags: List[String]
) extends Message
case class Remove(id: String)                    extends Message
case class AddToGui(id: String, data: Data)      extends Message
case class RemoveFromGui(id: String, data: Data) extends Message
case object Load                                 extends Message
