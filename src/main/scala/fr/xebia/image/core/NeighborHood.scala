package fr.xebia.image.core

/**
 * The base trait for the image processing functor.
 *
 * @tparam T the type of the pixel value
 */
trait NeighborHood[T] {

  def image: RawImage[T]

  /**
   * @param center the position of the target pixel
   * @return a list of #Position containing the positions of target pixels and all its neighbors
   */
  def neighborsAndSelf(center: Position): List[Position] =
    image.neighborsAndSelf(center)

  /**
   * @param pos the requested position
   * @return the pixel value at the requested position
   */
  def at(pos: Position): T = image.at(pos)

  /**
   * Return a new image processing where pixels at the specified positions have been replaced by
   * the specified pixel value.
   *
   * @param neighborList list of target position of pixels to be replaced
   * @param value the pixel value that will replace the specified positions values
   * @return a new image processing where pixels at the specified positions have the specified value
   */
  def replace(neighborList: List[Position], value: T): ImageFunctor[T] =
    ImageFunctor(image.replace(neighborList, value))

}
