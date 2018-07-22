package observatory

import java.time.LocalDate

import org.apache.spark.sql.Encoders

/**
  * 1st milestone: data extraction
  */
object Extraction extends Extraction with UsingSparkSession {

  private val stationsSchema = Encoders.product[Stations].schema
  private val temperatureSchema = Encoders.product[Temperatures].schema
  private val locationSchema = Encoders.product[Location].schema

  override def sparkLocateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[((Int,Int,Int), Location, Temperature)] = {

    val stations = if(USING_HDFS) {
      spark.read.schema(stationsSchema).csv(stationsFile)
    } else {
      spark.read.schema(stationsSchema).csv(getClass.getResource(stationsFile).getPath)
    }
    val temperatures = if(USING_HDFS) {
      spark.read.schema(temperatureSchema).csv(temperaturesFile)
    } else {
      spark.read.schema(temperatureSchema).csv(getClass.getResource(temperaturesFile).getPath)
    }

    val validStations = stations.select("stn","wban","lat","lon").where(stations("lat").isNotNull && stations("lon").isNotNull)
    val k = validStations.count
    val locations = validStations.join(temperatures,
        validStations("stn") <=> temperatures("stn") &&
        validStations("wban") <=> temperatures("wban"))
    import spark.implicits._
    locations.select("lat","lon","month","day","tempF").map { r =>
      ( (year, r.getAs[Int]("month"), r.getAs[Int]("day")),
        Location(r.getAs[Double]("lat"),r.getAs[Double]("lon")),
        (r.getAs[Double]("tempF") - 32.0) * 5.0/9.0)
    }.rdd.toLocalIterator.toSeq
  }

  override def sparkAverageRecords(records: Iterable[(Location, Temperature)]): Iterable[(Location, Temperature)] = {
    import spark.implicits._

    val t = records.groupBy[Location](_._1)
    t.foldLeft(List.empty[(Location,Temperature)]) { case (acc, it) => {
        val temps = it._2.toList
        val count = temps.length
        acc ++ foldLeftSum(temps).map {v => (v._1, v._2/count)}
      }
    }
  }

  def foldLeftSum[A](tuples: List[(A, Double)]) = tuples.foldLeft(Map.empty[A, Double])({
    case (acc, (k, v)) => acc + (k -> (v + acc.get(k).getOrElse(0.0)))
  }).toList
}

class Extraction extends Serializable {

  import org.apache.log4j.{Level, Logger}
  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  case class Stations(stn: String, wban: String, lat: Double, lon: Double)
  case class Temperatures(stn: String, wban: String, month: Int, day: Int, tempF: Double)

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */

  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    sparkLocateTemperatures(year, stationsFile, temperaturesFile).map { r =>
      (LocalDate.of(r._1._1,r._1._2,r._1._3), r._2, r._3)
    }
  }

  // Added method:
  def sparkLocateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[((Int,Int,Int), Location, Temperature)] = ???

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    val r = records.map { r => (r._2,r._3)}
    sparkAverageRecords(r)
  }

  // Added method:
  def sparkAverageRecords(records: Iterable[(Location, Temperature)]): Iterable[(Location, Temperature)] = ???

}
