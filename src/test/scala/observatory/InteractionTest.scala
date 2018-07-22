package observatory

import observatory.Interaction.{tileLocation}
import observatory.Visualization.{predictTemperature}
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

import scala.math._

trait InteractionTest extends FunSuite with Checkers {

  test("test zoom levels") {

    var tile = Tile(0,0,0)
    assert(equalToPrecision(tile.toLatLon.lat,85.0511,4))

    tile = Tile(0,0,1)
    assert(equalToPrecision(tile.toLatLon.lat,85.0511,4) && equalToPrecision(tile.toLatLon.lon,-180.0,4))
    tile = Tile(0,1,1)
    assert(equalToPrecision(tile.toLatLon.lat,0.0,4) && equalToPrecision(tile.toLatLon.lon,-180.0,4))
    tile = Tile(1,0,1)
    assert(equalToPrecision(tile.toLatLon.lat,85.0511,4) && equalToPrecision(tile.toLatLon.lon, 0.0,4))
    tile = Tile(1,1,1)
    assert(equalToPrecision(tile.toLatLon.lat,0.0,4) && equalToPrecision(tile.toLatLon.lon, 0.0,4))

    tile = Tile(0,0,2)
    assert(equalToPrecision(tile.toLatLon.lat,85.0511,4) && equalToPrecision(tile.toLatLon.lon,-180.0,4))
    tile = Tile(0,1,2)
    assert(equalToPrecision(tile.toLatLon.lat,66.51326044311186,5) && equalToPrecision(tile.toLatLon.lon,-180.0,4))
    tile = Tile(1,0,2)
    assert(equalToPrecision(tile.toLatLon.lat,85.0511,4) && equalToPrecision(tile.toLatLon.lon, -90.0,4))
    tile = Tile(1,1,2)
    assert(equalToPrecision(tile.toLatLon.lat,66.51326044311186,5) && equalToPrecision(tile.toLatLon.lon, -90.0,4))
    tile = Tile(2,2,2)
    assert(equalToPrecision(tile.toLatLon.lat,0.0,4) && equalToPrecision(tile.toLatLon.lon, 0.0,4))
    tile = Tile(3,3,2)
    assert(equalToPrecision(tile.toLatLon.lat,-66.51326044311186,5) && equalToPrecision(tile.toLatLon.lon, 90.0,4))
  }

  def equalToPrecision(value: Double, check: Double, precision: Int):Boolean = {

    val pFactor = pow(10,precision)
    val v1 = (value*pFactor).toInt
    val v2 = (check*pFactor).toInt
    v1 == v2
  }

  test("pixel geo coordinates") {
    var t = Tile(0,0,2)
    assert(equalToPrecision(t.getPixelLocation(0,0,256).lat,85.0511,4))
    assert(equalToPrecision(t.getPixelLocation(0,127,256).lat,79.23718500609334,5))
    assert(equalToPrecision(t.getPixelLocation(0,255,256).lat,66.65297740055279,5))
    t = Tile(0,1,2)
//    assert(equalToPrecision(Interaction.offsetToLatitude(t.y,0,t.zoom,256),66.51326044311186,5))
//    assert(equalToPrecision(Interaction.offsetToLatitude(t.y,255,t.zoom,256),0.35156029399227234,5))
    t = Tile(0,2,2)
//    assert(equalToPrecision(Interaction.offsetToLatitude(t.y,0,t.zoom,256),0.0,5))
//    assert(equalToPrecision(Interaction.offsetToLatitude(t.y,255,t.zoom,256),-66.37275500247456,5))
    t = Tile(0,3,2)
//    assert(equalToPrecision(Interaction.offsetToLatitude(t.y,0,t.zoom,256),-66.51326044311186,5))
//     assert(equalToPrecision(Interaction.offsetToLatitude(t.y,255,t.zoom,256),-85.02070774312594,5))
  }

  test("compute pixel offsets for temperature consistency checks") {

    // Tile(x,y,z) -> lon, lat, zoom
    var t = Tile(0,0,0)

    var pixel = latlonToPixel(0.0,0.0,t,256).getOrElse((-1,-1))
    assert(pixel._1 == 127, "the x offset should be ")
    assert(pixel._2 == 127, "the y offset should be ")

    t = Tile(0,0,1)
    pixel = latlonToPixel(0.0,0.0,t,256).getOrElse((-1,-1))
    assert(pixel._1 == 255, "the x offset should be 255")
    assert(pixel._2 == 255, "the y offset should be 255")

    t = Tile(1,0,1)
    pixel = latlonToPixel(0.0,0.0,t,256).getOrElse((-1,-1))
    assert(pixel._1 == 0, "the x offset should be 0")
    assert(pixel._2 == 255, "the y offset should be 255")

    t = Tile(0,1,1)
    pixel = latlonToPixel(0.0,0.0,t,256).getOrElse((-1,-1))
    assert(pixel._1 == 255, "the x offset should be 255")
    assert(pixel._2 == 0, "the y offset should be 0")

    t = Tile(1,1,1)
    pixel = latlonToPixel(0.0,0.0,t,256).getOrElse((-1,-1))
    assert(pixel._1 == 0, "the x offset should be 0")
    assert(pixel._2 == 0, "the y offset should be 0")

    t = Tile(0,0,2)
    pixel = latlonToPixel(0.0,0.0,t,256).getOrElse((-1,-1))
    assert(pixel._1 == -1, "the x offset should be -1")
    assert(pixel._2 == -1, "the y offset should be -1")

    t = Tile(0,0,2)
    pixel = latlonToPixel(0.0,0.0,t,256).getOrElse((-1,-1))
    assert(pixel._1 == -1, "the x offset should be -1")
    assert(pixel._2 == -1, "the y offset should be -1")

    t = Tile(1,1,2)
    pixel = latlonToPixel(0.0,0.0,t,256).getOrElse((-1,-1))
    assert(pixel._1 == 255, "the x offset should be 0")
    assert(pixel._2 == 255, "the y offset should be 0")

    t = Tile(2,1,2)
    pixel = latlonToPixel(0.0,0.0,t,256).getOrElse((-1,-1))
    assert(pixel._1 == 0, "the x offset should be 0")
    assert(pixel._2 == 255, "the y offset should be 0")

    t = Tile(1,2,2)
    pixel = latlonToPixel(0.0,0.0,t,256).getOrElse((-1,-1))
    assert(pixel._1 == 255, "the x offset should be 0")
    assert(pixel._2 == 0, "the y offset should be 0")

    t = Tile(2,2,2)
    pixel = latlonToPixel(0.0,0.0,t,256).getOrElse((-1,-1))
    assert(pixel._1 == 0, "the x offset should be 0")
    assert(pixel._2 == 0, "the y offset should be 0")

  }

  test("check temperature consistency at 0.0 across zoom levels") {

    val tiles = Seq(
      Tile(0,0,0),
      Tile(0,0,1),
      Tile(1,0,1),
      Tile(0,1,1),
      Tile(1,1,1),
      Tile(1,1,2),
      Tile(2,1,2),
      Tile(1,2,2),
      Tile(2,2,2)
    )

    var map = scala.collection.mutable.Map.empty[Tile,(Int,Int)]
    tiles.foreach(t =>
      map.put(t, latlonToPixel(0.0,0.0,t,256).getOrElse((-1,-1)))
    )

    val t0 = Seq(
      Tile(0,0,0)
    )

    val t1 = Seq(
      Tile(0,0,1),
      Tile(1,0,1),
      Tile(0,1,1),
      Tile(1,1,1)
    )

    val t2 = Seq(
    Tile(1,1,2),
    Tile(2,1,2),
    Tile(1,2,2),
    Tile(2,2,2)
    )

    val ref = Seq(
      (Location(45.0,-90.0),20.0),
      (Location(45.0,90.0),0.0),
      (Location(0.0,0.0),10.0),
      (Location(-45.0,-90.0),0.0),
      (Location(-45.0,90.0),20.0)
    )


    val temp0 = predictTemperatureAtPixel(ref,Tile(0,0,0),127,127)
    val temp1 = t1.foldLeft(0.0) { (temp, tile) =>
      var loc = map.get(tile).getOrElse((-1,-1))
      temp + predictTemperatureAtPixel(ref,tile,loc._1,loc._2)
    }
    assert((temp0*1000).toInt == (temp1/4.0*1000).toInt, "the same point should be the same temp")

    val temp2 = t2.foldLeft(0.0) { (temp, tile) =>
      var loc = map.get(tile).getOrElse((-1,-1))
      temp + predictTemperatureAtPixel(ref,tile,loc._1,loc._2)
    }
    assert((temp0*1000).toInt == (temp1/4.0*1000).toInt, "the same point should be the same temp")

    val (x0,y0) = latlonToPixel(45.0,45.0,Tile(0,0,0),256).getOrElse((-1,-1))
    val (x1,y1) = latlonToPixel(45.0,45.0,Tile(1,0,1),256).getOrElse((-1,-1))
    val (x2,y2) = latlonToPixel(45.0,45.0,Tile(2,1,2),256).getOrElse((-1,-1))
    val (x3,y3) = latlonToPixel(45.0,45.0,Tile(4,2,3),256).getOrElse((-1,-1))
    val zz = Interaction.tileLocation(Tile(4,2,3))
    val lat0 = Tile(0,0,0).getPixelLocation(x0,y0,256).lat
    val lat1 = Tile(1,0,1).getPixelLocation(x1,y1,256).lat
    val lat2 = Tile(2,1,2).getPixelLocation(x2,y2,256).lat
    val lat3 = Tile(4,2,3).getPixelLocation(x3,y3,256).lat
    val temp00 = predictTemperatureAtPixel(ref,Tile(0,0,0),x0,y0)
    val temp10 = predictTemperatureAtPixel(ref,Tile(1,0,1),x1,y1)
    val temp20 = predictTemperatureAtPixel(ref,Tile(2,1,2),x2,y2)
    val temp30 = predictTemperatureAtPixel(ref,Tile(4,2,3),x3,y3)

    assert(abs(temp00 - temp10) < 0.3)
    assert(abs(temp00 - temp20) < 0.3)
    assert(abs(temp00 - temp30) < 0.3)
  }

  def predictTemperatureAtPixel( temperatures: Iterable[(Location,Temperature)],
                 tile: Tile, tx: Int, ty: Int): Temperature = {
    // size of a tile
    val ptemps = Array.ofDim[Temperature](256,256)

    (0 until 256).foreach { y =>
      (0 until 256).par.foreach { x =>
        val ploc = tile.getPixelLocation(x,y,256)
        ptemps(x)(y) = predictTemperature(temperatures, ploc);
      }
    }
    ptemps(tx)(ty)
  }

  def latlonToPixel(lat: Double, lon: Double, tile: Tile, res: Int): Option[(Int,Int)] = {
    val location = tileLocation(tile)
    val zoomFactor = (1<<tile.zoom).toDouble
    val lonStep = 360.0 / (res).toDouble / zoomFactor

    val lonOffset = lon - location.lon
    val latOffest = lat - location.lat

    val x = if(location.lon.toInt - lon.toInt == 0) 0 else (lonOffset/lonStep - 0.5).toInt

    val y = if(location.lat.toInt - lat.toInt == 0) 0
    else {
      val ypp = tan(toRadians(lat))
      val yp = log(ypp + sqrt(pow(ypp,2)+1))
      val yi = ((1.0 - yp/Pi) * zoomFactor)/2.0
      (yi * res.toDouble - 0.5).toInt % res
    }
    if( x > 255 || y > 255) None else Some((x,y))
  }

}
