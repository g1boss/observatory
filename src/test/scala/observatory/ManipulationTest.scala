package observatory

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

trait ManipulationTest extends FunSuite with Checkers {

  test ("grid maker function test") {
    val temperatures1 = Seq(
      (Location(-17.533, 118.95), 26.717913204062803),
      (Location(19.267, -69.75), 10.0),
      (Location(19.7, -87.9), 27.27633685558761)
    )

    val temperatures2 = Seq(
      (Location(-17.0, 118.0), 0.0),
      (Location(19.0, -69.0), -5.0),
      (Location(19.5, -87.0), 10.0)
    )

    val gtemps1 = Manipulation.makeGrid(temperatures1)
    val gtemps2 = Manipulation.makeGrid(temperatures2)
    val gridLocation = GridLocation(19,-70)

    assert((gtemps1(gridLocation)*1000).toInt === 10000, "not the correct temperature at this grid location")
    assert(gtemps2(gridLocation) === -4.999999373197597, "not the correct temperature at this grid location")
    // and if we ask again
    assert((gtemps1(gridLocation)*1000).toInt === 10000, "not the correct temperature at this grid location")
  }

  test ("average temperature grid test") {

    val yearlyTemperatureSets = Seq (
      Seq(
        (Location(-17.533, 118.95), 26.717913204062803),
        (Location(19.267, -69.75), 26.92237442922376),
        (Location(19.7, -87.9), 27.27633685558761)
      ),

      Seq(
        (Location(-17.0, 118.0), 0.0),
        (Location(19.0, -69.0), -5.0),
        (Location(19.5, -87.0), 10.0)
      )
    )
    val getAverageTemperature = Manipulation.average(yearlyTemperatureSets)

    val gridLocation = GridLocation(19,-70)

    assert(getAverageTemperature(gridLocation) == (26.922374429254987 - 4.999999373197597)/2.0, "not the correct average temperature")
  }

  test ("no deviation") {

    val yearlyTemperatureSets = Seq (
      Seq(
        (Location(-17.0, 118.0), 0.0),
        (Location(19.0, -69.0), -5.0),
        (Location(19.5, -87.0), 10.0)
      ),

      Seq(
        (Location(-17.0, 118.0), 0.0),
        (Location(19.0, -69.0), -5.0),
        (Location(19.5, -87.0), 10.0)
      )
    )

    val reports = Seq (
        (Location(-17.0, 118.0), 0.0),
        (Location(19.0, -69.0), -5.0),
        (Location(19.5, -87.0), 10.0)
    )


    val getAverageTemperature = Manipulation.average(yearlyTemperatureSets)
    val gridLocation = GridLocation(19,-70)

    val delta = Manipulation.deviation(reports, getAverageTemperature)

    assert(delta(gridLocation) == 0.0, "not the correct average temperature")
  }

  test ("hit an average deviation") {

    val yearlyTemperatureSets = Seq (
      Seq(
        (Location(-17.0, 118.0), 0.0),
        (Location(19.0, -69.0), -5.0),
        (Location(19.5, -87.0), 10.0)
      ),

      Seq(
        (Location(-17.0, 118.0), 10.0),
        (Location(19.0, -69.0), 5.0),
        (Location(19.5, -87.0), 20.0)
      )
    )

    val reports = Seq (
      (Location(-17.0, 118.0), 5.0),
      (Location(19.0, -69.0), 0.0),
      (Location(19.5, -87.0), 15.0)
    )


    val getAverageTemperature = Manipulation.average(yearlyTemperatureSets)
    val gridLocation = GridLocation(19,-70)

    val delta = Manipulation.deviation(reports, getAverageTemperature)

    assert(delta(gridLocation).toInt == 0, "not the correct average temperature")
  }

  test("grid values") {
    val ref = Seq(
      (Location(45.0,-90.0),20.0),
      (Location(45.0,90.0),0.0),
      (Location(0.0,0.0),10.0),
      (Location(-45.0,-90.0),0.0),
      (Location(-45.0,90.0),20.0)
    )

    val grid = Manipulation.makeGrid(ref)
    val gridLocation1 = GridLocation(-45,90)
    val gridLocation2 = GridLocation(88, 56)
    val gridtemp1 = grid(gridLocation1)
    val gridtemp2 = grid(gridLocation2)
    assert(gridtemp1 >= 10.0 && gridtemp1 <= 20.0)
    assert(gridtemp2 >= 5.0 && gridtemp2 <= 10.0)
    println(s"gridtemp1: $gridtemp1, gridtemp2: $gridtemp2")
  }

}
