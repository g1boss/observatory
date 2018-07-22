package observatory

import java.io.File

import com.sksamuel.scrimage.{Image, Pixel, RGBColor}
import observatory.Visualization.{interpolateColor, predictTemperature}

import scala.math.{Pi, atan, sinh, _}
import collection.mutable.{HashMap, MultiMap, Set}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

//  var index: Map[Int,Int]= Map.empty[Int,Int]
  val IMAGE_SIZE = 128
  val ZOOM = 3
  val MAKE_COMPLETE_ZOOM_SET = true
  val LATTITUDE_SPAN_DEGREES = 170.10225755961318

  /**
    * One performance key here is to ensure that this routine can work with Iterable[(Location,Temperature)]
    * that has a minimal set of values that satisfies generating an image.  A second option is to reduce
    * the image quality by adjusting IMAGE_PIXEL_WIDTH and IMAGE_PIXEL_HT
    *
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {

    val pixels = makePixels(temperatures,colors,tile)
    Image(IMAGE_SIZE,IMAGE_SIZE, pixels.flatten)
  }

  def makePixels(temperatures: Iterable[(Location,Temperature)],
                 colors: Iterable[(Temperature,Color)],
                 tile: Tile): Array[Array[Pixel]] = {
    // size of a tile
    val pixels = Array.ofDim[Pixel](IMAGE_SIZE,IMAGE_SIZE)
    val ptemps = Array.ofDim[Temperature](IMAGE_SIZE,IMAGE_SIZE)

    (0 until IMAGE_SIZE).foreach { y =>
      (0 until IMAGE_SIZE).par.foreach { x =>
        val ploc = tile.getPixelLocation(x,y,IMAGE_SIZE)
        ptemps(x)(y) = predictTemperature(temperatures, ploc);
        val pixel = interpolateColor(colors, ptemps(x)(y));
        pixels(y)(x) = Pixel(RGBColor(pixel.red, pixel.green, pixel.blue, 127))
      }
    }
    pixels
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */

  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit = {

    // compute the subset of Data required to compute each tile
    yearlyData.foreach { yr =>
//      val index = indexLocationSequence(yr._2.asInstanceOf[Iterable[(Location,Temperature)]])
      // generate the tile set at the configured ZOOM
      val zStop = ZOOM+1
      val zStart = if(MAKE_COMPLETE_ZOOM_SET) 0 else ZOOM
      (zStart until zStop).foreach { z =>
        val zoomDimension = (1<<z)

        (0 to (zoomDimension - 1)).map { y =>
//          val temperatures = getLatitudeRings(index,Tile(0,y,z))
            val temperatures = yr._2.asInstanceOf[Iterable[(Location,Temperature)]]

          (0 to (zoomDimension - 1)).par.map { x =>
            val tile = Tile(x,y,z)
            generateImage(yr._1,Tile(x,y,z),temperatures.toSeq.asInstanceOf[Data])
          }
        }
      }
    }
  }

  def generateImage[Data](year: Year, t: Tile, temperatures: Data): Unit = {
    val path = s"target/temperatures/${year}/${t.zoom}/"
    val fileName = path + s"${t.x}-${t.y}.png"
    val fileSystem = new File(path)
    fileSystem.mkdirs()
    println(fileName)
    val image = tile(temperatures.asInstanceOf[Iterable[(Location,Temperature)]], Visualization.COLOR_TEMPS, t)
    image.output(fileName)
    ()
  }

  /**
    * A wrapper that converts tile coordinates to map coordinates
    *
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    Location(tile.toLatLon.lat,tile.toLatLon.lon)
  }

  /**
    * Extracts a subset of the temperature data to speed up tile generation.
    * Uses a mulimap of temperature data subsets organized by latitude.
    *
    * This speeds up tile generation in generateTiles()
    *
    * @param index
    * @param tile
    * @return
    */
  def getLatitudeRings(index: MultiMap[Double,(Location,Temperature)], tile: Tile)
    : Iterable[(Location,Temperature)] = {
    // starting location and zoom
    val location = tileLocation(tile)

    val lat = location.lat.toInt
    val zoom = tile.zoom
    val span = lat - toDegrees(atan(sinh(Pi * (1.0 - 2.0 * (tile.y + 1).toDouble / (1<<zoom))))).toInt
    val padding = if(span/2 < 20) span/2 else 20

    val maxLat = LATTITUDE_SPAN_DEGREES.toInt / 2
    val startLat = if(lat + max(padding,2) > maxLat)
      maxLat else lat + max(padding,2)
    val stopLat = if(lat - span - max(padding + 2,2) < -1 * maxLat)
      -1 * maxLat else lat - span - max(padding + 2,2)

    val temperatures = for {
      x <- startLat to stopLat by - 1
      if(!index.get(x).isEmpty)
    } yield index.get(x).toSeq
    val result = temperatures.flatten.flatten
    result.toList
  }

  /**
    * Organizes the temperature iterable into a map of sub-intervals separeted by latitude degree
    * @param temperatures
    * @return
    */
  def indexLocationSequence(temperatures: Iterable[(Location,Temperature)]): MultiMap[Double,(Location,Temperature)] = {

    val temperatureIndex = new HashMap[Double, Set[(Location,Temperature)]] with MultiMap[Double, (Location,Temperature)]
    temperatures.foldLeft(temperatureIndex) { (index, report) => index.addBinding(report._1.lat.toInt, report) }
  }
}
