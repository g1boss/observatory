package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Interaction.IMAGE_SIZE
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

trait Visualization2Test extends FunSuite with Checkers {

  test("bilinear interpolation") {

    val point = CellPoint(0.5,0.5)
    val d00 = 10.0
    val d10 = 0.0
    val d01 = 10.0
    val d11 = 0.0

    var estimate = Visualization2.bilinearInterpolation(point,d00,d10,d01,d11)
    assert(estimate == 5.0)
  }

  test("make images") {
    val ref = Seq(
      (Location(45.0,-90.0),20.0),
      (Location(45.0,90.0),0.0),
      (Location(0.0,0.0),10.0),
      (Location(-45.0,-90.0),0.0),
      (Location(-45.0,90.0),20.0)
    )
    val i = Visualization2.visualizeGrid(Manipulation.makeGrid(ref), Visualization.COLOR_TEMPS, Tile(0,0,0))
    Visualization2.visualizeGrid(Manipulation.makeGrid(ref), Visualization.COLOR_TEMPS, Tile(0,0,1))
    Visualization2.visualizeGrid(Manipulation.makeGrid(ref), Visualization.COLOR_TEMPS, Tile(1,0,1))
    Visualization2.visualizeGrid(Manipulation.makeGrid(ref), Visualization.COLOR_TEMPS, Tile(0,1,1))
    Visualization2.visualizeGrid(Manipulation.makeGrid(ref), Visualization.COLOR_TEMPS, Tile(1,1,1))
    Visualization2.visualizeGrid(Manipulation.makeGrid(ref), Visualization.COLOR_TEMPS, Tile(0,0,0))
  }

  test("check grid resolution") {

   val nomralLat = for (x <- -90 to 90) yield Visualization2.offset(x,3)
   val uniqueLat = nomralLat.toSet.toList.sorted
    assert(uniqueLat.size === 61)
    val nomralLon = for (x <- -90 to 90) yield Visualization2.offset(x,3)
    val uniqueLon = nomralLat.toSet.toList.sorted
    println(uniqueLon.size === 180)
  }
}
