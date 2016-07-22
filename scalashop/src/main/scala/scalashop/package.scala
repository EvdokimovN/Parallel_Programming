
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    // TODO implement using while loops
    if (radius == 0) return rgba(x, y, x + y, math.abs(x - y))
    println(src.apply(x,y))
    var R = 0
    var G = 0
    var B = 0
    var A = 0
    var n = 0
    var i = radius
    /*
      Loop calculates average in a left to right, top to bottom manner,
      starting from row y+radius and column x+radius
      and ending on row y-radius and columns x-radius
     */
    while (i >= -radius){
        var j = radius
        val newY = y + i
        while (j >= -radius){
          var newX = x + j
          if (newY > src.height || newX > src.width || newY < 0 || newX < 0){
              j -= 1
          }
          else {
            val pixel = src.apply(newX, newY)
            R += red(pixel)
            G += green(pixel)
            B += blue(pixel)
            A += alpha(pixel)
            n += 1
            j -= 1}

        }
      i -= 1
    }
    rgba(R/n, G/n, B/n, A/n)

  }

}
