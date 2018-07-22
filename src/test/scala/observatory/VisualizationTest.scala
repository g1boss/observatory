package observatory


import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import scala.math._

trait VisualizationTest extends FunSuite with Checkers {

  test("test special color range limits") {
    var scale = List((-1.0,Color(255,0,0)), (100.0,Color(0,0,255)))
    var color = Visualization.interpolateColor(scale, -11.0)
    assert(colormatch((255,0,0),color), "should match test value")
    scale = List((0.0,Color(255,0,0)), (2.147483647E9,Color(0,0,255)))
    color = Visualization.interpolateColor(scale, 5.3687091175E8)
    assert(colormatch((191,0,64),color), "should match test value")
    scale = List((76.53132409285678,Color(255,0,0)), (100.0,Color(0,0,255)))
    color = Visualization.interpolateColor(scale, 66.53132409285678)
    assert(colormatch((255,0,0),color), "should match test value")
  }

  test("exact temperature interpolations") {

    var color = Visualization.interpolateColor(Visualization.COLOR_TEMPS, -61.0)
    assert(colormatch((0,0,0),color), "color below minimum temp should be minimum temp color")
    color = Visualization.interpolateColor(Visualization.COLOR_TEMPS, -60.0)
    assert(colormatch((0,0,0),color), "color capped at minimum temp")
    color = Visualization.interpolateColor(Visualization.COLOR_TEMPS, -0.0)
    assert(colormatch((0, 255, 255),color), "color for -0.0 should be 0.0")
    color = Visualization.interpolateColor(Visualization.COLOR_TEMPS, 0.0)
    assert(colormatch((0, 255, 255),color), "color 0.0 should be 0.0")
    color = Visualization.interpolateColor(Visualization.COLOR_TEMPS, 60.0)
    assert(colormatch((255, 255, 255),color), "color should be max temp")
    color = Visualization.interpolateColor(Visualization.COLOR_TEMPS, 61.0)
    assert(colormatch((255, 255, 255),color), "color capped at max temp")
    color = Visualization.interpolateColor(Visualization.COLOR_TEMPS, 22.0)
    assert(colormatch((255,128,0),color), "color should be halfway between 12 and 32")
  }

  def colormatch(c1: (Int,Int,Int), c2: Color): Boolean = {
    val b = c1._1 == c2.red && c1._2 == c2.green && c1._3 == c2.blue
    b
  }

  test("check a few distances") {
    val ref1 = Location(42.990967, -71.463767)
    val ref2 = Location(0.0, -90.0)
    val ref3 = Location(89.9, 0.0)

    var delta = Visualization.distance(ref1,Location(42.990967, -70.463767))
    assert( abs(delta.getOrElse(0.0)) - 81.55 < 0.1,  "the distance " + delta + "should be close to 81.55 km")
    delta = Visualization.distance(ref1,Location(43.990967, -71.463767))
    assert( abs(delta.getOrElse(0.0) - 111.1) < 0.1, "the distance " + delta + "should be close to 111.1 km")
    delta = Visualization.distance(ref1,Location(43.990967, -71.463767))
    assert( abs(delta.getOrElse(0.0) - 111.1) < 0.1, "the distance " + delta + "should be close to 111.1 km")
  }

  test("check a few temperature estimates") {
    var temperatures = Seq(
    (Location(-17.533,118.95),26.717913204062803),
    (Location(19.267,-69.75),26.92237442922376),
    (Location(19.7,-87.9),27.27633685558761)
    )
    var temp = Visualization.predictTemperature(temperatures, Location(19.7,-87.9))
    assert(temp == 27.27633685558761, "an exact location should have an exact temperature")
    temp = Visualization.predictTemperature(temperatures, Location(-17.533,118.85))
    assert(temp == 26.7179132040628, "the temperature should be the average of the local samples")

    temperatures = Seq(
      (Location(-17.533,118.95),0.0),
      (Location(19.267,-69.75),26.92237442922376),
      (Location(19.7,-87.9),27.27633685558761)
    )
    temp = Visualization.predictTemperature(temperatures, Location(19.7,-87.9))
    assert(temp == 27.27633685558761, "an exact location should have an exact temperature")
    temp = Visualization.predictTemperature(temperatures, Location(-17.533,118.85))
    assert(temp.round == 0.0, "the temperature should be the average of the local samples")
    temp = Visualization.predictTemperature(temperatures, Location(0.0,0.0))
    assert(temp == 26.009781533734138, "the temperature should be the average of the local samples")

    temperatures = Seq(
      (Location(-5.0,-5.0),0.0),
      (Location(-5.0,5.0),-10.0),
      (Location(5.0,0.0),10.0)
    )
    temp = Visualization.predictTemperature(temperatures, Location(0.0,0.0))
    assert(temp == 6.990835907589044, "an exact location should have an exact temperature")
    temp = Visualization.predictTemperature(temperatures, Location(0.0,2.5))
    assert(temp == 0.0, "the temperature should be the average of the local samples")
    temp = Visualization.predictTemperature(temperatures, Location(0.0,-2.5))
    assert(temp == 4.5835651112998566, "the temperature should be the average of the local samples")

  }

  test("make a chunky image") {
    val temperatures = Seq(
      (Location(-17.533,118.95),26.717913204062803),
      (Location(19.267,-69.75),26.92237442922376),
      (Location(19.7,-87.9),27.27633685558761)
    )
    var image = Visualization.visualize(temperatures, Visualization.COLOR_TEMPS)
    image.output(new java.io.File("target/image1.png"))
    assert(true, "don't be silly")
  }

  test("try to make a full image") {
    val temperatures = Seq(
      (Location(-17.533,118.95),26.717913204062803),
      (Location(19.267,-69.75),26.92237442922376),
      (Location(19.7,-87.9),27.27633685558761)
    )
    var image = Visualization.visualize(temperatures, Visualization.COLOR_TEMPS)
    image.output(new java.io.File("target/image1.png"))
    assert(true, "don't be silly")
  }

  test("tile consistency across zoom levels") {
    val temperatures = Seq(
      (Location(45.0,-90.0),20.0),
      (Location(45.0,90.0),0.0),
      (Location(0.0,0.0),10.0),
      (Location(-45.0,-90.0),0.0),
      (Location(-45.0,90.0),20.0)
    )
  }

}
