package observatory

import java.io.File

import com.sksamuel.scrimage.{Image, Pixel, RGBColor}
import observatory.Interaction.{IMAGE_SIZE, MAKE_COMPLETE_ZOOM_SET, ZOOM}
import observatory.Visualization.interpolateColor
import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.fs.{FileSystem, Path}

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  val GRID_RESOLUTION = 3 // only works with 1 or 3

  val DEVIATION_COLOR_TEMPS = Seq[(Temperature,Color)](
    (7, Color(0,0,0)),
    (4, Color(255,0,0)),
    (2, Color(255,255,0)),
    (0, Color(255,255,255)),
    (-2, Color(0,255,255)),
    (-7, Color(0,0,255))
  )

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    point: CellPoint,
    d00: Temperature, // ul
    d01: Temperature, // ur
    d10: Temperature, // ll
    d11: Temperature  // lr
  ): Temperature = {
    d00 + (d10 - d00) * point.x + (d01 - d00) * point.y + (d11 + d00 - (d10 + d01)) * point.x * point.y
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param tile Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: GridLocation => Temperature,
    colors: Iterable[(Temperature, Color)],
    tile: Tile
  ): Image = {

    val pixels = Array.ofDim[Pixel](IMAGE_SIZE,IMAGE_SIZE)
    val ptemps = Array.ofDim[Temperature](IMAGE_SIZE,IMAGE_SIZE)

    // estimate the pixels of one tile
    (0 until IMAGE_SIZE).foreach { y =>
      (0 until IMAGE_SIZE).par.foreach { x =>    // par here
        val ploc = tile.getPixelLocation(x,y,IMAGE_SIZE)

        val ptemp = predictTemperature(ploc, grid, GRID_RESOLUTION);
        ptemps(x)(y) = ptemp
        val pixel = interpolateColor(colors, ptemps(x)(y));
        pixels(y)(x) = Pixel(RGBColor(pixel.red, pixel.green, pixel.blue, 127))
      }
    }
    Image(IMAGE_SIZE,IMAGE_SIZE, pixels.flatten)
  }

  def predictTemperature(l: Location, grid: GridLocation => Temperature, gridRes: Int=1): Temperature = {

    val abslat = Math.abs(l.lat).toInt
    val y1 = if(abslat > 88) 90 else abslat + 1
    val (north,south) = if (l.lat < 0)
      (-1 * (y1-1), -1 * y1)
    else
      (y1, y1-1)

    val abslon = Math.abs(l.lon).toInt
    val x1 = if(abslon > 178) 180 else abslon + 1
    val (west,east) = if(l.lon < 0 )
      (-1 * x1,-1 * (x1-1))
    else
      (x1-1, if(x1 > 179) -180 else x1)

    val deltay = if(l.lat > 0.0) north - l.lat else Math.abs(l.lat - north)
    val deltax = if(l.lon > 0.0) Math.abs(west - l.lon) else l.lon - west

    bilinearInterpolation(
      CellPoint(deltay,deltax),
      grid(GridLocation(offset(north, gridRes),offset(west, gridRes))),
      grid(GridLocation(offset(north, gridRes),offset(east, gridRes))),
      grid(GridLocation(offset(south, gridRes),offset(west, gridRes))),
      grid(GridLocation(offset(south, gridRes),offset(east, gridRes)))
    )
  }

  def offset(x: Int, f: Int = 1): Int = {
    if(x % f == 0) x else {
      if(x>0) {
        if(x % f > 1) Math.min(x+1,179) else x-1
      } else {
        if(x % f < -1) x-1 else x+1
      }
    }
  }

  def generateTiles[Data](year: Int, temperatures: GridLocation => Temperature, colors: Iterable[(Temperature,Color)], hadoopConf: Configuration, fsPrefix: String): Unit = {

    println(s"start tile generation for $year.")
    val zStop = ZOOM+1
    val zStart = if(MAKE_COMPLETE_ZOOM_SET) 0 else ZOOM
    (zStart until zStop).foreach { z =>
      val zoomDimension = (1<<z)
      (0 to (zoomDimension - 1)).map { y =>
        (0 to (zoomDimension - 1)).map { x => // par here?
          makeTile(Tile(x,y,z),year,temperatures,hadoopConf,fsPrefix)
        }
      }
    ()
    }
  }

  def makeTile[Data](tile: Tile, year: Int, temperatures: GridLocation => Temperature, hadoopConf: Configuration, fsPrefix: String): Unit = {
    val path = s"${fsPrefix}" + s"/target/deviations/${year}/${tile.zoom}/"
    val name = s"${tile.x}-${tile.y}.png"

    val image = Visualization2.visualizeGrid(temperatures, DEVIATION_COLOR_TEMPS,tile)

    if(fsPrefix.startsWith("hdfs")) {
      writeToFile(image.bytes, path, name, hadoopConf)
    } else {
      val localpath = s"${fsPrefix}observatory/target/deviations/${year}/${tile.zoom}/"
      val fileSystem = new File(localpath)
      fileSystem.mkdirs()
      val fileName = localpath + name
      val file = new File(fileName)
      println(file.getAbsolutePath)
      image.output(file)
    }
    println(s"zoom level ${tile.zoom} --> ${new java.util.Date} -> ${path}")
  }

  def writeToFile(bytes: Array[Byte], base: String, fileName: String, hadoopConf: Configuration): Unit = {

    val fs = FileSystem.get(hadoopConf)
    val path = new Path(base + "/" + fileName)

    if (fs.exists(path)) {
      fs.delete(path,false)
    }
    println("create file " + path.toString)
    val os = fs.create(path)
    os.write(bytes)
    os.flush()
    os.close()
  }
}
