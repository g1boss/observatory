package observatory

import scala.math._

/**
  * Introduced in Week 1. Represents a location on the globe.
  * @param lat Degrees of latitude, -90 ≤ lat ≤ 90
  * @param lon Degrees of longitude, -180 ≤ lon ≤ 180
  */
case class Location(lat: Double, lon: Double) {
  def toGridLocation(): GridLocation = {
    GridLocation(lat.toInt,lon.toInt)
  }
}

/**
  * Introduced in Week 3. Represents a tiled web map tile.
  * See https://en.wikipedia.org/wiki/Tiled_web_map
  * Based on http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
  * @param x X coordinate of the tile
  * @param y Y coordinate of the tile
  * @param zoom Zoom level, 0 ≤ zoom ≤ 19
  */
case class Tile(x: Int, y: Int, zoom: Int) {

  // true slippy-map coordinates, which overlap tile start points...
  // not used in this project
  def toLatLon = new LatLonPoint(
    toDegrees(atan(sinh(Pi * (1.0 - 2.0 * y.toDouble / (1<<zoom))))),
    x.toDouble / (1<<zoom) * 360.0 - 180.0,
    zoom)

  def getPixelLocation(xoffset: Int, yoffset: Int, resolution: Int = 1): Location = {
    val resFactor = (log(resolution)/log(2)).toInt
    new Location(
      toDegrees(atan(sinh(Pi * (1.0 - 2.0 * (y.toDouble*resolution.toDouble + yoffset.toDouble) / (1<<(zoom+resFactor)))))),
      ((x.toDouble*resolution.toDouble + xoffset.toDouble)/resolution.toDouble) / (1<<zoom) * 360.0 - 180.0)
  }

  def toURI = new java.net.URI("http://tile.openstreetmap.org/"+zoom+"/"+x+"/"+y+".png")

}

case class LatLonPoint(lat: Double, lon: Double, z: Int) {

  def toTile = new Tile(
    ((lon + 180.0) / 360.0 * (1 << z)).toInt,
    ((1 - log(tan(toRadians(lat)) + 1 / cos(toRadians(lat))) / Pi) / 2.0 * (1 << z)).toInt,
    z)
}

/**
  * Introduced in Week 4. Represents a point on a grid composed of
  * circles of latitudes and lines of longitude.
  * @param lat Circle of latitude in degrees, -89 ≤ lat ≤ 90
  * @param lon Line of longitude in degrees, -180 ≤ lon ≤ 179
  */
case class GridLocation(lat: Int, lon: Int) {

  override def toString():String = {
  s"$lat-$lon"
  }
}

/**
  * Introduced in Week 5. Represents a point inside of a grid cell.
  * @param x X coordinate inside the cell, 0 ≤ x ≤ 1
  * @param y Y coordinate inside the cell, 0 ≤ y ≤ 1
  */
case class CellPoint(x: Double, y: Double)

/**
  * Introduced in Week 2. Represents an RGB color.
  * @param red Level of red, 0 ≤ red ≤ 255
  * @param green Level of green, 0 ≤ green ≤ 255
  * @param blue Level of blue, 0 ≤ blue ≤ 255
  */
case class Color(red: Int, green: Int, blue: Int)

