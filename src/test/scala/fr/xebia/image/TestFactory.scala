package fr.xebia.image

import fr.xebia.image.core.RawImage.Matrix
import fr.xebia.image.core.{ImageFunctor, Position, RawImage}
import fr.xebia.image.export.ImageWriter

import scala.util.Try

object TestFactory {

  case class ImageBuilder() {
    def filledWith(aContent: Matrix[String]): Try[RawImage[String]] =
      RawImage.buildFrom[String](aContent)
  }

  def aStringImage = ImageBuilder()

  def anImageFunctorFrom(rawContent: String): ImageFunctor[String] = {
    val contents: Matrix[String] = rawContent
      .split("\n")
      .map(_.toCharArray.toList.map(_.toString))
      .toList
      .filter(_.map(_.trim).mkString.nonEmpty)
    RawImage.buildFrom(contents).map(img => ImageFunctor(img)).get
  }

  object ImagingTools {

    def aSeedThatMatches(imageFunctor: ImageFunctor[String], position: Position, expectedValue: String): Position = {
      val firstSeed = imageFunctor
        .firstThatMatches("#")
        .getOrElse(throw new IllegalStateException("# not found"))
      assert(firstSeed == position)
      assert(imageFunctor.at(firstSeed) == "#")
      firstSeed
    }

    def writeToFile(
      imageFunctor: ImageFunctor[String],
      front: List[Position],
      fileName: String,
      newContent: String = "@"
    ): Unit = {
      val newImage = imageFunctor.replace(front, newContent).image
      ImageWriter.writeToFile(fileName, newImage)
    }

  }

}
