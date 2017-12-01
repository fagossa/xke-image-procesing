package fr.xebia.image.core

import fr.xebia.image.core.RawImage.Matrix

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

case class Position(x: Int, y: Int) extends Ordered[Position] {
  override def toString: String = s"($x,$y)"

  import scala.math.Ordered.orderingToOrdered

  override def compare(that: Position): Int = (this.x, this.y) compare(that.x, that.y)
}

object RawImage {

  type Matrix[U] = List[List[U]]

  def buildFrom[T](content: Matrix[T]): Try[RawImage[T]] = {
    content match {
      case Nil =>
        Failure(new IllegalArgumentException(s"Empty content is not supported"))

      case list if list.map(_.size).distinct.size != 1 =>
        Failure(new IllegalArgumentException(s"Only squared matrix are supported"))

      case _ =>
        Success(new RawImage(content))
    }
  }

}

/**
  * Generic image, as a matrix of pixels.
  *
  * @param content list of list of pixels.
  * @tparam T the type of the pixel value.
  */
final case class RawImage[T] private(content: Matrix[T]) {

  val width = content.head.size

  val height = content.size

  /**
    * Get the first element that matches following a left-right / up-down strategy
    */
  def firstThatMatches(searched: T): Option[Position] = {
    val zipped: List[(Int, List[T])] = content.indices.toList.zip(content)
    zipped
      .find { case (_, xValues) => xValues.contains(searched) }
      .map { case (y, xValues) => Position(xValues.indexOf(searched), y) }
  }

  /**
    * Replace the pixels at the specified position by the specified pixel value.
    *
    * @param neighborList the position where pixel value must be replaced
    * @param value        the new pixel value to erase the specified neighborList with
    * @return an updated image with specified pixels replaced by specified value.
    */
  def replace(neighborList: List[Position], value: T): RawImage[T] = {
    @tailrec
    def go(updatedImage: RawImage[T], remainingNeighbor: List[Position]): RawImage[T] = {
      remainingNeighbor match {
        case Nil =>
          updatedImage

        case currentPos :: remainingPositions =>
          go(
            new RawImage[T](content = replaceValueInContent(currentPos, updatedImage, value)),
            remainingPositions
          )
      }
    }

    go(this, neighborList)
  }

  private def replaceValueInContent(position: Position, currentImage: RawImage[T], newValue: T): Matrix[T] = {
    val valueReplaced = currentImage.content(position.y).updated(position.x, newValue)
    currentImage.content.updated(
      position.y,
      valueReplaced
    )
  }

  /**
    * @param center searched position
    * @return all the neighbors pixels of the searched position
    */
  def neighborsOnly(center: Position): List[Position] = {
    val neigh = scala.collection.mutable.ArrayBuffer.empty[Position]
    neigh += center.copy(x = center.x - 1, y = center.y - 1)
    neigh += center.copy(x = center.x - 1)
    neigh += center.copy(x = center.x - 1, y = center.y + 1)
    neigh += center.copy(y = center.y + 1)
    neigh += center.copy(y = center.y + 1, x = center.x + 1)
    neigh += center.copy(y = center.y - 1)
    neigh += center.copy(y = center.y - 1, x = center.x + 1)
    neigh += center.copy(x = center.x + 1)

    neigh.toList
      .filter(pos => pos.x >= 0 && pos.y >= 0)
      .filter(pos => pos.x < width && pos.y < height)
  }

  /**
    * @param center searched position
    * @return the position of the specified <code>center</code> and all its neighbors pixels
    */
  def neighborsAndSelf(center: Position): List[Position] =
    neighborsOnly(center) :+ center

  def at(pos: Position): T = content(pos.y)(pos.x)

}
