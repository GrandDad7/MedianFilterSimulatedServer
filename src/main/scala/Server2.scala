package Exam2

import java.awt.Color
import java.awt.image.BufferedImage
import akka.actor.Actor
import scala.collection.parallel.mutable.ParArray

case class Server2Response(image: BufferedImage, duration: Long)

class Server2 extends Actor {

  def receive = {
    case image: BufferedImage => {
      val beforeTime = System.currentTimeMillis()
      val newImage = MedianFilterParallel(image)
      val afterTime = System.currentTimeMillis()

      sender() ! Server2Response(newImage, afterTime - beforeTime)
    }
  }

  def clamp(n: Int, min: Int, max: Int): Int = { // ensure when corners are grabbed are not negatives
    math.min(max, math.max(n, min))
  }

  def cloneImage(image: BufferedImage): BufferedImage = {
    val newImage = new BufferedImage(image.getWidth, image.getHeight, image.getType)
    val graphics = newImage.getGraphics
    graphics.drawImage(newImage, 0, 0, null)
    graphics.dispose()
    newImage
  }

  def getMedian(image: BufferedImage, x: Int, y: Int): Color = {
    var R = new Array[Int](9)
    var G = new Array[Int](9)
    var B = new Array[Int](9)

    val imagePixels = new ParArray[Color](9)

    val h = image.getHeight()
    val w = image.getWidth()

    imagePixels(0) = new Color(image.getRGB(clamp(x-1, 0, w-1), clamp(y-1, 0, h-1)))
    imagePixels(1) = new Color(image.getRGB(clamp(x-1, 0, w-1), clamp(y, 0, h-1)))
    imagePixels(2) = new Color(image.getRGB(clamp(x-1, 0, w-1), clamp(y+1, 0, h-1)))
    imagePixels(3) = new Color(image.getRGB(clamp(x, 0, w-1), clamp(y+1, 0, h-1)))
    imagePixels(4) = new Color(image.getRGB(clamp(x+1, 0, w-1), clamp(y+1, 0, h-1)))
    imagePixels(5) = new Color(image.getRGB(clamp(x+1, 0, w-1), clamp(y, 0, h-1)))
    imagePixels(6) = new Color(image.getRGB(clamp(x+1, 0, w-1), clamp(y-1, 0, h-1)))
    imagePixels(7) = new Color(image.getRGB(clamp(x, 0, w-1), clamp(y-1, 0, h-1)))
    imagePixels(8) = new Color(image.getRGB(clamp(x, 0, w-1), clamp(y, 0, h-1)))


    for(i<-0 to 8) {
      R(i) = imagePixels(i).getRed()
      G(i) = imagePixels(i).getGreen()
      B(i) = imagePixels(i).getBlue()
    }

    R = R.sorted
    G = G.sorted
    B = B.sorted

    new Color(R((R.size - 1)/2), G((G.size - 1) / 2), B((B.size - 1) / 2))
  }

  def MedianFilterParallel(image: BufferedImage): BufferedImage = {
    val newImage = cloneImage(image)

    for (xy: Int <- (0 until (image.getWidth() * image.getHeight())).par) {
      val x: Int = xy % image.getWidth()
      val y: Int = math.floor(xy / image.getWidth()).toInt
      val color = getMedian(image, x, y)
      newImage.setRGB(x, y, color.getRGB)
    }

    newImage
  }

}