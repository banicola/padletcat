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
import scala.collection.mutable.Map
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

object Main extends JFXApp {

  private implicit val timeout = Timeout(15, SECONDS)

  val system = ActorSystem("MyActorSys")

  // creating the bacht simulator Actor.
  val simul = system.actorOf(
    Props(new BachTSimul()),
    "simul"
  )

  val guiActor = system.actorOf(
    Props(new GUIActor(simul)),
    "guiActor"
  )

  Thread.sleep(5000)

  val response = simul ? Init

  val username = Await.result(response, timeout.duration).asInstanceOf[String]

  simul ! guiActor

  val grilleContenu = new FlowPane(20, 20) {
    style = "-fx-background-color:transparent"
  }

  case class Filter(val name: String, val padlet: String)

  var datas             = ListBuffer[(String, Data)]()
  var filterButtonsList = List[ToggleButton]()
  var filters: ListBuffer[Filter] = ListBuffer(
    Filter("CatLover", "CatLover"),
    Filter("GrumpyCat", "GrumpyCat"),
    Filter("Peanut&Butter", "Peanut&Butter")
  )
  var padletNames = new ListBuffer[String]();
  padletNames += "CatLover"
  padletNames += "GrumpyCat"
  padletNames += "Peanut&Butter"
  // Padlet spinner in create content
  var padletCombox = new ComboBox(padletNames) {
      minWidth = 200
      maxWidth = 200
    } 
  var comboBox_items = FXCollections.observableArrayList[String]();
  padletCombox.setItems(comboBox_items);
  updatePadletNames(padletNames)

  val toggleGroup: ToggleGroup    = new ToggleGroup();

  def updatePadletNames(padletListNames: ListBuffer[String]):Unit={
    comboBox_items.clear();
    padletNames.foreach( x => comboBox_items.add(x))
  }

  guiActor ! Load

  stage = new PrimaryStage {
    title = "PadletCat"
    width = 1920
    height = 1080

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
            padletNames -= button.getText
            updatePadletNames(padletNames)
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

      val logo     = new Image("padletcat.png")
      val logoView = new ImageView(logo)

      val logoHBox = new HBox(20)
      val style =
        "-fx-background-color: #0066FF; -fx-text-fill: #FFFFFF; -fx-font-weight: bold; -fx-font-family: Helvetica Neue; -fx-font-size: 18px;"
      logoHBox.setStyle(style)
      logoHBox.setPadding(new Insets(10, 0, 0, 0))

      logoHBox.children = List(logoView)

      val profileImg  = new Image("profile.png")
      val profileView = new ImageView(profileImg)

      val profile = new Button(username) {
        graphic = profileView
        style =
          "-fx-background-color: #0066FF; -fx-text-fill: #FFFFFF; -fx-font-weight: bold; -fx-font-family: Helvetica Neue; -fx-font-size: 18px;"
      }

      profile.margin_=(new Insets(20))
      profile.setAlignment(Pos.CenterRight)

      val headerHBox = new BorderPane {
        style = "-fx-background-color: rgba(0, 102, 255); "
      }
      //headerHBox.setPrefHeight(70)
      headerHBox.setPadding(new Insets(10, 20, 10, 40))
      headerHBox.left = logoHBox
      headerHBox.right = profile

      // HEADER END -----------------------------------

      // Content begin ---------------------------------------------------------------

      val styleFilterSelected =
        "-fx-background-color: #0066FF; -fx-text-fill: #FFFFFF; -fx-font-family: Helvetica Neue; -fx-font-size: 18px; -fx-background-radius: 5 5 5 5"
      val styleFilterHovered =
        "-fx-background-color: rgba(0, 102, 255,0.3); -fx-border-color: rgba(255,255,255,0); -fx-text-fill: #0066FF; -fx-font-family: Helvetica Neue; -fx-font-size: 18px; -fx-background-radius: 5 5 5 5"
      val styleFilterNotSelected =
        "-fx-background-color: rgba(255,255,255,0); -fx-border-color: rgba(255,255,255,0); -fx-text-fill: #0066FF; -fx-font-family: Helvetica Neue; -fx-font-size: 18px; -fx-background-radius: 5 5 5 5"

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
          "-fx-background-color: rgba(255,255,255,0); -fx-border-color: rgba(255,255,255,0); -fx-text-fill: #0066FF; -fx-font-family: Helvetica Neue; -fx-font-size: 18px; -fx-background-radius: 5 5 5 5"
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
          "-fx-background-color: rgba(255,255,255,0); -fx-border-color: rgba(255,255,255,0); -fx-text-fill: #0066FF; -fx-font-family: Helvetica Neue; -fx-font-size: 18px; -fx-background-radius: 5 5 5 5"
        padding = new Insets(10)

      }

      val cancelEditFilters = new Button {
        graphic = cancelImgView
        style =
          "-fx-background-color: rgba(255,255,255,0); -fx-border-color: rgba(255,255,255,0); -fx-text-fill: #0066FF; -fx-font-family: Helvetica Neue; -fx-font-size: 18px; -fx-background-radius: 5 5 5 5"
        padding = new Insets(10)
      }

      val addFilter = new Button {
        graphic = addImgView
        style =
          "-fx-background-color: rgba(255,255,255,0); -fx-border-color: rgba(255,255,255,0); -fx-text-fill: #0066FF; -fx-font-family: Helvetica Neue; -fx-font-size: 18px; -fx-background-radius: 5 5 5 5"
        padding = new Insets(10)
      }

      // ################################## FILTERS POP UP BEGIN ##################################

      val filterPanesStyle =
        "-fx-background-color: rgb(255,255,255); -fx-background-radius: 10 10 10 10"

      val filterBoxTitle = new Label("Créer un padlet") {
        style =
          "-fx-background-color: rgb(255,255,255); -fx-text-fill: #0066FF; -fx-font-family: Helvetica Neue; -fx-font-size: 22px;"
      }

      val closeImage     = new Image("close.jpg")
      val closeImageView = new ImageView(closeImage)
      closeImageView.onMouseClicked = new EventHandler[MouseEvent] {
        override def handle(event: MouseEvent) {
          mainStack.children.remove(addStackFilter)
        }
      }

      val blueLineFilter = new Line {
        stroke = rgb(0, 102, 255)
        strokeWidth = 2
        startX = 31
        endX = 930
      }

      val addFilterTopPane = new BorderPane
      addFilterTopPane.setStyle("-fx-background-color: rgb(255,255,255)")
      addFilterTopPane.setPadding(new Insets(0, 0, 15, 0))

      addFilterTopPane.left = filterBoxTitle
      addFilterTopPane.right = closeImageView
      addFilterTopPane.bottom = blueLineFilter

      val addFilterName = new TextField {
        minWidth = 400
        maxWidth = 500
      }

      val labelTitre = new Label("Entrez le titre")

      val vboxTitre = new VBox
      vboxTitre.children = List(labelTitre, addFilterName)

      /*
      val padlet = new ComboBox(padletName) {
        minWidth = 185
        maxWidth = 185
      }
      val labelPadlet = new Label("Padlet")
      val vboxPadlet  = new VBox
      vboxPadlet.children = List(labelPadlet, padlet)
      */

      val contentAddFilterPane = new HBox(20)
      contentAddFilterPane.setStyle("-fx-background-color: rgb(255,255,255);")
      contentAddFilterPane.setPadding(new Insets(0, 30, 0, 30))
      //contentAddFilterPane.children = List(vboxTitre, vboxPadlet)
      contentAddFilterPane.children = List(vboxTitre)

      val greyLineFilter = new Line {
        stroke = rgb(150, 150, 150)
        strokeWidth = 1
        startX = 31
        endX = 930
      }

      val createButton = new Button("Créer") {
        style =
          "-fx-background-color: #0066FF; -fx-text-fill: #FFFFFF; <font-weight>: regular; -fx-font-family: Helvetica Neue; -fx-font-size: 16px; -fx-border-color: #0066FF; -fx-background-radius: 5 5 5 5"
        minWidth = 120
        maxWidth = 120
        minHeight = 30
        maxHeight = 35
      }

      val cancelButton = new Button("Annuler") {
        style =
          "-fx-background-color: #FFFFFF; -fx-text-fill: #0066FF; <font-weight>: regular; -fx-font-family: Helvetica Neue; -fx-font-size: 16px; -fx-border-color: #0066FF; -fx-background-radius: 5 5 5 5"
        minWidth = 120
        maxWidth = 120
        minHeight = 30
        maxHeight = 35
      }

      cancelButton.onMouseClicked = new EventHandler[MouseEvent] {
        override def handle(event: MouseEvent) {
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
          if (addFilterName.text.value != "") {
            filters += Filter(
              addFilterName.text.value,
              addFilterName.text.value
            )
            filterButtonsList = createFiltersButton()
            filterButtonsList.map(_.setToggleGroup(toggleGroup))
            filtersHBox.children = filterButtonsList
            padletNames += addFilterName.text.value
            updatePadletNames(padletNames)

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

        val blueLine = new Line {
          stroke = rgb(0, 102, 255)
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
            mainStack.children.remove(ajoutContenu)
          }
        }
        
        val addFilter     = new Image("add.jpg")
        val addFilterView = new ImageView(addFilter)
        addFilterView.onMouseClicked = new EventHandler[MouseEvent] {
          override def handle(event: MouseEvent) {}
        }

        val upload     = new Image("upload.png")
        val uploadView = new ImageView(upload)

        val addContent = new VBox(20)

        val topBar = new BorderPane
        val ajoutTitle = new Text {
          text = "Proposer un nouveau contenu"
          style = "-fx-font-size: 22pt"
          fill = rgb(0, 102, 255)
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

        val labelPadlet = new Label("Padlet")
        val vboxPadlet  = new VBox
        vboxPadlet.children = List(labelPadlet, padletCombox)

        values.children =
          List(vboxTitre, vboxPadlet)

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
            "-fx-background-color: #ffffff; -fx-border-color: #0066FF; -fx-text-fill: #0066FF;"
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
            "-fx-background-color: #ffffff; -fx-border-color: #0066FF; -fx-text-fill: #0066FF;"
          cancelButton = true
        }
        cancelButton.onAction = new EventHandler[ActionEvent] {
          override def handle(event: ActionEvent) {
            mainStack.children.remove(ajoutContenu)
          }
        }
        val publishButton = new Button("Publier") {
          style =
            "-fx-background-color: #0066FF; -fx-border-color: #0066FF; -fx-text-fill: #ffffff;"
        }
        publishButton.onAction = new EventHandler[ActionEvent] {
          override def handle(event: ActionEvent) {
            val dateFormatter        = new SimpleDateFormat("dd/MM/yyyy hh:mm aa")
            var submittedDateConvert = new Date()
            val date                 = dateFormatter.format(submittedDateConvert)
            var encodedfile: String  = ""

            if (titre.text.value != "" && description.text.value != "" && padletCombox.value.value != null) {
              if (selectedImage != null) {
                //transform file to string

                encodedfile =
                  Base64.getEncoder().encodeToString(Files.readAllBytes(selectedImage.toPath()))

              }

              guiActor ! Add(
                username,
                date,
                encodedfile,
                video.text.value,
                titre.text.value,
                description.text.value,
                List(padletCombox.value.value)
              )

              mainStack.children.remove(ajoutContenu)

            } else {
            }

          }
        }
        bottomButtons.children = List(cancelButton, publishButton)

        val bottomBar = new BorderPane
        bottomBar.right = bottomButtons

        addContent.children = List(
          topBar,
          blueLine,
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
        x._2.padlet.contains(filter.padlet)
      }
    } else {
      datas.toList
    }

    for ((id, data) <- filteredDatas.reverse) {
      grilleContenu.children += contentCell(
        id,
        data.author,
        data.date,
        data.title,
        data.text,
        data.image,
        data.video
      )
    }
  }

  def contentCell(
      contentID: String,
      author: String,
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
        if (username == author) {
          guiActor ! Remove(contentID)
        } else {
          println("L'utilisateur n'a pas la permission de supprimer ce contenu")
        }
      }
    }

    val deleteHBox = new BorderPane
    deleteHBox.right = deleteButtonView

    val userLabel = new Text {
      text = author
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
      fill = rgb(0, 102, 255)
      style = "-fx-font-size: 22pt"
      wrappingWidth = 500
    }

    val texte = new Text(text) {
      fill = rgb(72, 72, 72)
      style = "-fx-font-size: 14pt"
      wrappingWidth = 500
    }

    val videoLabel = new Text {
      text = video
      fill = rgb(102, 163, 255)
      style = "-fx-font-size: 10pt"
      wrappingWidth = 500
    }

    val topBar = new BorderPane
    topBar.left = userLabel
    topBar.right = dateLabel

    val contentPane = new VBox(10) {
      maxWidth = 500
    }
    if (username == author) {
      contentPane.children += deleteHBox
    }
    contentPane.children += topBar
    contentPane.children += titleLabel
    contentPane.children += texte
    contentPane.children += videoLabel
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
      system.terminate()
    }
  }
}

class GUIActor(simul: ActorRef) extends Actor {

  def addContent(
      author: String,
      date: String,
      image: String,
      video: String,
      title: String,
      text: String,
      padlet: List[String]
  ) = {
    val content: Data = Data(
      author,
      date,
      image,
      video,
      title,
      text,
      padlet
    )
    val x = Random.nextInt(Integer.MAX_VALUE)
    println(x.toString)
    simul ! Command(s"tell($x)", Map(x.toString -> content))
  }

  def receive = {
    case Add(
        author: String,
        date: String,
        image: String,
        video: String,
        title: String,
        text: String,
        padlet: List[String]
        ) =>
      addContent(author, date, image, video, title, text, padlet)
    case Remove(id: String) => simul ! Command(s"get($id)", Map[String,Data]())
    case Load               => simul ! Command("get(All)", Map[String,Data]())
    case x: Data            =>
      println(s"le get a supprimé l'élément $x")
    case x: String =>
      println(s"Le tell s'est bien effectué et a ajouté un objet avec l'id: $x")
    case AddToGui(id: String, data: Data) =>
      // ici, on peut j'ajouter à la GUI si il est dans les bonnes catégorie (cfr: filtres)
      Platform.runLater(
        Main.addToGui(id, data)
      )
    case RemoveFromGui(id: String) =>
      Platform.runLater(
        Main.removeFromGui(id)
      )
    case _ => println("received unknown message")
  }
}

sealed trait Message
case class Add(
    author: String,
    date: String,
    image: String,
    video: String,
    title: String,
    text: String,
    padlet: List[String]
) extends Message
case class Remove(id: String)                    extends Message
case class AddToGui(id: String, data: Data)      extends Message
case class RemoveFromGui(id: String) extends Message
case object Load                                 extends Message