package observatory

import com.sksamuel.scrimage.{Image, Pixel, RGBColor}

import scala.annotation.tailrec
import scala.math._

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  val COLOR_TEMPS = Seq[(Temperature,Color)](
    (60.0, Color(255, 255, 255)),
    (32.0, Color(255, 0, 0)),
    (12.0, Color(255, 255, 0)),
    (0.0, Color(0, 255, 255)),
    (-15.0, Color(0, 0, 255)),
    (-27.0, Color(255, 0 , 255)),
    (-50.0, Color(33, 0, 107)),
    (-60.0, Color(0, 0, 0))
  )

  val P = 6.0
  val R = 6371.0 // earth's radius in km
  val halfAWorldAway = Pi*R
  val RANGE_LIMIT = halfAWorldAway// km

  case class Accumulator(weightedSumDistance: Double, sumDistanceWeight: Double, closest: (Double, Temperature))

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    val acc = Accumulator(0.0,0.0,(halfAWorldAway, 0.0))
    val result: Accumulator = accumulateTemperatureData(temperatures, location, acc)
    result.weightedSumDistance / result.sumDistanceWeight
  }

  @tailrec
  def accumulateTemperatureData(temperatures: Iterable[(Location,Temperature)],
                                location: Location, acc: Accumulator): Accumulator = temperatures match {
    case Nil => if(acc.sumDistanceWeight == 0.0) // none < RANGE_LIMIT, use closest
      Accumulator(pow(acc.closest._1,P) * acc.closest._2, pow(acc.closest._1,P), acc.closest)
      else acc
    case s :: rest => {
      val refLocation = s._1
      val refTemp = s._2
      val d = distance(refLocation, location).getOrElse(0.0)

      if (d <= 1.0) {
        // exact match
        Accumulator(refTemp, 1.0, acc.closest)
      } else {

        // save the closest since we may not find any < RANGE_LIMIT
        val closest = if(d < acc.closest._1) (d, refTemp) else acc.closest

          val distanceMetric = pow(d,-P)
          // accumulate this one
          val nextAcc = Accumulator(acc.weightedSumDistance + distanceMetric * refTemp, acc.sumDistanceWeight + distanceMetric, closest)
          accumulateTemperatureData(rest, location, nextAcc)
      }
    }
  }

  def distance(ref: Location, target: Location): Option[Double] = (ref,target) match {
    case (Location(lat1,lon1),Location(lat2,lon2)) if lat1 == lat2 && lon1 == lon2 => Some(0.0)
    case (Location(lat1,lon1),Location(lat2,lon2)) if abs(lat1 - lat2) == 180.0 && abs(lon1 - lon2) == 180.0 => Some(halfAWorldAway)
    case (Location(lat1,lon1),Location(lat2,lon2)) => {
      val dLat=(lat2 - lat1).toRadians
      val dLon=(lon2 - lon1).toRadians

      val a = pow(sin(dLat/2),2) + pow(sin(dLon/2),2) * cos(lat1.toRadians) * cos(lat2.toRadians)
      val c = 2 * asin(sqrt(a))
      Some(R * c)
    }
    case _ => None
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  @tailrec
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {

    // the sequence is always ordered least to most
    val sortedPoints = points.toSeq.sortBy(x => x._1)

    sortedPoints match {
      case Nil => Color(0,0,0)
      case s :: rest
        // value is >= last item in the list
        if value == s._1 || rest.isEmpty =>
          mapColor(s, s, value)
      case s :: rest
        // value is <= smallest item in the list
        //if s._1 < 0 && value < s._1 =>
        if value < s._1 =>
          mapColor(s, s, value)
      case s :: rest
        // the value is between the head and the first item on rest
        if between(s._1, value, rest.head._1) =>
          if( s._1 < value)
            mapColor(s,rest.head,value)
          else
            mapColor(rest.head,s,value)
      case s :: rest =>
        // try next larger item on list
        interpolateColor(rest, value)
    }
  }

  def between(x1: Double, y: Double, x2: Double): Boolean = {
    (x1 < y && x2 > y) || (x1 < y && x2 > y)
  }

  def mapColor(low: (Temperature, Color), high: (Temperature, Color), value: Temperature): Color = {
    val delta = if(high._1-low._1 == 0.0) 0.0 else {
      val inc = (value - low._1)
      val span = (high._1 - low._1)
      inc/span
    }
    //    val delta = if(high._1-low._1 == 0.0) 0.0 else (high._1 - value)/(high._1-low._1)
    Color(
      (low._2.red + (high._2.red - low._2.red) * delta).round.toInt,
      (low._2.green + (high._2.green - low._2.green) * delta).round.toInt,
      (low._2.blue + (high._2.blue - low._2.blue) * delta).round.toInt
    )
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {

    val pixels = Array.ofDim[Pixel](180,360)

    val latRange = 90 until -90 by -1
    val lonRange = -180 until 180 by 1

    latRange.par.foreach { lat =>
      lonRange.par.foreach { lon =>
        val pixel = interpolateColor(colors, predictTemperature(temperatures, Location(lat.toDouble, lon.toDouble)))
        pixels((90 - lat) % 180)((lon + 180) % 360) = Pixel(RGBColor(pixel.red, pixel.green, pixel.blue))
      }
    }
    Image(360,180, pixels.flatten)
  }

}

