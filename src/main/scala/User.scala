import java.awt.{Color, Font, Graphics, Image}
import java.awt.image.BufferedImage
import java.io.File

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.duration._
import Exam2.{Server1, Server1Response, Server2, Server2Response}
import javax.swing.{JComponent, JFileChooser, JFrame}
import javax.swing.filechooser.FileNameExtensionFilter
import javax.imageio.ImageIO

import scala.concurrent.Future
import scala.util.{Failure, Success}

object User extends App {

  def saveToNewImage(img: BufferedImage, fileName: String): Unit = {
    ImageIO.write(img, "jpg", new File("./output/" + fileName + ".jpg"))
  }

  def loadImageFile: BufferedImage = {
    val choose = new JFileChooser
    val filter = new FileNameExtensionFilter("JPG & PNG Images", "jpg", "png")
    choose.setCurrentDirectory(new File("."))
    choose.setDialogTitle("Select an image file")
    choose.setFileFilter(filter)
    if (choose.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
      return ImageIO.read(choose.getSelectedFile)
    }
    throw new Exception("You did not select an image.")
    null
  }

  def cloneImage(image: BufferedImage): BufferedImage = {
    val newImage = new BufferedImage(image.getWidth, image.getHeight, BufferedImage.TYPE_INT_RGB)
    val graphics = newImage.createGraphics()
    graphics.drawImage(image, 0, 0, null)
    graphics.dispose()
    newImage
  }

  val system = ActorSystem()
  val serverSerial: ActorRef = system.actorOf(Props[Server1], name = "Server1")
  val serverParallel: ActorRef = system.actorOf(Props[Server2], name = "Server2")

  val originalImage: BufferedImage = loadImageFile
  val filteredImage: BufferedImage = cloneImage(originalImage)

  implicit val timeout = Timeout(10000.second)
  implicit val ec = system.dispatcher
  val mfSerial: Future[Any] = serverSerial ? filteredImage
  val mfParallel: Future[Any] = serverParallel ? filteredImage

  val gotTheImages: Future[(Any, Any)] = for {
    imageSerial <- mfSerial
    imageParallel <- mfParallel
  } yield (imageSerial, imageParallel)

  gotTheImages.onComplete {
    case Success(res) =>
      val npDuration = res._1.asInstanceOf[Server1Response].duration
      val pDuration = res._2.asInstanceOf[Server2Response].duration


      println("Median Filter Serial: " + npDuration + "ms")
      println("Median Filter Parallel: " + pDuration + "ms")

      val mfSerial = res._1.asInstanceOf[Server1Response].image
      val mfParallel = res._2.asInstanceOf[Server2Response].image

      val displaySerial: FilteredImage = new FilteredImage(mfSerial, npDuration)
      val displayParallel: FilteredImage = new FilteredImage(mfParallel, pDuration)

      saveToNewImage(mfSerial, "mfSerial")
      saveToNewImage(mfSerial, "mfParallel")

      println("Images saved in the output folder.")

      val photos = List(displaySerial, displayParallel)


      val frame: JFrame = new JFrame("Frame")
      frame.add(new Window(photos))
      frame.setSize(photos.length * filteredImage.getWidth * 2, photos.length * filteredImage.getHeight + 50)
      frame.setVisible(true)
      frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

      system.terminate()
    }
    case Failure(e) => e.printStackTrace
  }

  class FilteredImage(img: BufferedImage, time: Long) {
    def getImg: BufferedImage = img
    def getTime: Long = time
  }

  class Window(var images: List[FilteredImage]) extends JComponent {

    override def paintComponent(g: Graphics): Unit = {
      super.paintComponent(g)
      g.drawImage(originalImage, 50, 100, null)
      g.setFont(new Font("Arial", Font.BOLD, 30))
      g.setColor(Color.GREEN)
      g.drawString("Original Image", filteredImage.getWidth / 2 - 50, 90)

      for (i: Int <- images.indices) {
        val img: FilteredImage = images(i)
        val x: Int = i % 2
        val y: Int = math.floor(i / 2).toInt
        g.drawImage(img.getImg, x * img.getImg.getWidth + 50 * (x + 1) + img.getImg.getWidth + 50, y * img.getImg.getHeight + 50 * (y + 2), null)
        g.setFont(new Font("Arial", Font.BOLD, 14))
        g.setColor(Color.BLUE)
        g.drawString("This image took " + img.getTime + " milliseconds", x * img.getImg.getWidth + 50 * (x + 1) + img.getImg.getWidth + 50, y * img.getImg.getHeight + 46 * (y + 2))
      }
    }
  }

}