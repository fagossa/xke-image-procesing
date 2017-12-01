package fr.xebia.image.core

import scala.annotation.tailrec

/**
 * A functor to process images. It extends {@link Propagation} with operations dedicated to the detection of connected elements.
 *
 * @param image
 * @tparam T the type of the pixel value
 */
case class ImageFunctor[T](image: RawImage[T]) extends NeighborHood[T] {

  def firstThatMatches(searched: T): Option[Position] = image.firstThatMatches(searched)

  /**
   * Count the number of components in this processed image and replaces all connected pixels in those components by an "empty pixel" marker.
   * Example: if <code>contentValue</code is '@', the following image has a connected elements count of 2. There is 2 distinguishable objects composed of connected '@'.
   * <pre>
   * ..........
   * ..@@..@@@.
   * ..@@...@..
   * ..@@...@..
   * ..........
   * </pre>
   *
   * @param contentValue the value that connected pixels must match
   * @param emptyValue  the pixel value that will mark processed pixels
   * @return the number of components in this processed image (a component is a set of connected pixels).
   */
  def countConnectedElements(contentValue: T, emptyValue: T): Int = {
    @tailrec
    def go(copyImage: RawImage[T], maybePosition: Option[Position], connectedElements: Int): Int = {
      maybePosition match {
        case None =>
          connectedElements

        case Some(seed) =>
          val connectedPoints = propagateFront(
            copyImage.neighborsAndSelf(seed),
            contentValue,
            emptyValue
          )
          val newImage = copyImage.replace(connectedPoints, emptyValue)
          go(newImage, newImage.firstThatMatches(contentValue), connectedElements + 1)
      }
    }
    go(this.image, image.firstThatMatches(contentValue), 0)
  }

  private[image] def propagateFront(seed: Position, searchedValue: T, markWith: T): List[Position] = {
    val seeds = neighborsAndSelf(seed)
    propagateFront(seeds, searchedValue, markWith)
  }

  /*
   * Helper function to find all connected pixels with the specified value.
   * @param seeds a list of position to start the lookup. Positions must be neighbors in order to find only connected pixels.
   * @param searchedValue the value that all connected pixels must match
   * @param markWith the "empty pixel" marker that will replace pixels once processed.
   * @return the list of positions of the connected pixels (i.e. belonging to a single component)
   */
  private[image] def propagateFront(seeds: List[Position], searchedValue: T, markWith: T): List[Position] = {
    @tailrec
    def go(imageCopy: RawImage[T], neighbors: List[Position], positions: List[Position]): List[Position] = {
      neighbors.filter(imageCopy.at(_) == searchedValue) match {
        case Nil =>
          positions
        case currentSeed :: remainingSeed =>
          val newNeighborhood: List[Position] = imageCopy.neighborsAndSelf(currentSeed)
          val newImage = imageCopy.replace(List(currentSeed), markWith)
          go(newImage, remainingSeed ++ newNeighborhood, positions :+ currentSeed)
      }
    }
    go(image, seeds, List.empty[Position])
  }

  /**
   * Replace pixels that match the specified predicate.
   *
   * @param predicate the predicate
   * @param replaceBy the pixel value that will replace pixels matching predicate <code>p</code>
   * @return a new processing functor where pixels fulfilling <code>p</code> have been replaced by <code>replaceBy</code>
   */
  def threshold(predicate: T => Boolean, replaceBy: T): ImageFunctor[T] =
    map[T](cell => if (predicate(cell)) replaceBy else cell)

  /**
   * Change image
   *
   * @param f the mapping function (converts a pixel of one type to another)
   * @tparam R the type of the pixel value in the resulting image
   * @return the processing functor for the converted image
   */
  def map[R](f: T => R): ImageFunctor[R] =
    new ImageFunctor[R](
      image.copy(
        image.content.map(_.map(cell => f(cell)))
      )
    )

}
