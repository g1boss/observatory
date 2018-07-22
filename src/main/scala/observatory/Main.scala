package observatory

import observatory.Interaction.{MAKE_COMPLETE_ZOOM_SET, ZOOM}
import observatory.Manipulation.makeGrid
import org.apache.hadoop.fs.{FileSystem, Path}

object Main extends App with UsingSparkSession {

  var inPrefix = "hdfs:///user/hadoop/observatory"
  var outPrefix = "/home/hadoop/"

  if(args.length > 0) {
    inPrefix = if (args(0).endsWith("/")) args(0).substring(0, args(0).lastIndexOf('/')) else args(0)
    outPrefix = if (args.length > 1) {
      if (args(1).endsWith("/")) args(1).substring(0, args(1).lastIndexOf('/')) else args(1)
    } else inPrefix
  }

  val BASELINE_START_YEAR = 1975
  val BASELINE_END_YEAR = 1976 //1989
  val DEVIATION_START_YEAR = 1991
  val DEVIATION_END_YEAR = 1992 //2015

  object Mode extends Enumeration {
    type mode = Value
    val Baseline, Averages, Deviations = Value
  }

  val MODE = Mode.Deviations
  val extraction = Extraction

//  @tailrec
  def getYearlyTemps(year: Int, maxYear: Int, yearlyDatasets: List[(Int,Iterable[(Location,Temperature)])])
  : List[(Int,Iterable[(Location,Temperature)])] =
    year match {
      case y if y< maxYear+1 => {
        println(y)
        val tempSeq = extraction.locateTemperatures(year, inPrefix + "/stations.csv", inPrefix + "/" + year + ".csv")
        val yearlyDataset = (y,extraction.locationYearlyAverageRecords(tempSeq))
        getYearlyTemps(y + 1, maxYear, yearlyDatasets :+ yearlyDataset)
      }
      case _ => yearlyDatasets
    }

//  @tailrec
  def getBaselineTemperatures(year: Int, maxYear: Int, yearlyDatasets: List[Iterable[(Location,Temperature)]])
  : List[Iterable[(Location,Temperature)]] =
    year match {
      case y if y < maxYear => {
        println(y)
        val tempSeq = extraction.locateTemperatures(year, inPrefix +  "/stations.csv", inPrefix + "/" + year + ".csv")
        val yearlyDataset = (extraction.locationYearlyAverageRecords(tempSeq))
        getBaselineTemperatures(y + 1, maxYear, yearlyDatasets :+ yearlyDataset)
      }
      case _ => yearlyDatasets
    }

  def getTemperatureGrid(getTemperatureAtLocation: GridLocation => Temperature): scala.collection.immutable.Map[String,Temperature] = {
    val init = scala.collection.immutable.Map.empty[String,Temperature]

    def getLatitudeRingGrid(init: scala.collection.immutable.Map[String,Temperature], lat: Int):
    scala.collection.immutable.Map[String,Temperature] =  {
      val lonRange = -180 to 179
      lonRange.foldLeft[scala.collection.immutable.Map[String,Temperature]](init) { (m2,lon) =>
        m2 + (GridLocation(lat,lon).toString -> getTemperatureAtLocation(GridLocation(lat,lon)))
      }
    }

    val latRange = spark.sparkContext.parallelize(-89 to 90)
    latRange.aggregate[scala.collection.immutable.Map[String,Temperature]](init) (
      getLatitudeRingGrid(_,_), _++_
    )
  }

  println(new java.util.Date())

  MODE match {
    case m if m == Mode.Deviations => {
      println("making deviation tiles")
      println(new java.util.Date())

      val temperatureList = getYearlyTemps(BASELINE_START_YEAR,BASELINE_END_YEAR,List.empty[(Int,Iterable[(Location,Temperature)])])
      val yearlyTemps =
        temperatureList.foldLeft[Seq[Iterable[(Location,Temperature)]]](Seq.empty[Iterable[(Location,Temperature)]])
          { (l,x) => l :+ x._2 }
      //      val yearlyTemps = getBaselineTemperatures(BASELINE_START_YEAR,BASELINE_END_YEAR+1,List.empty[Iterable[(Location,Temperature)]])
      val averages = Manipulation.average(yearlyTemps)

      println("compute average grid points")
      println(new java.util.Date())
      val avgGrid = getTemperatureGrid(averages)
      val bcastAvgGrid = spark.sparkContext.broadcast[scala.collection.immutable.Map[String,Temperature]](avgGrid)
      println(new java.util.Date())

      println("marshalling observation dataset")
      println(new java.util.Date())
      val observations = getYearlyTemps(DEVIATION_START_YEAR,DEVIATION_END_YEAR,List.empty[(Int,Iterable[(Location,Temperature)])])

      println(new java.util.Date())

      println("create observation yearly grid")

      val gridYearlyTemp = observations
        .aggregate[scala.collection.immutable.Map[Int,scala.collection.immutable.Map[String, Temperature]]](scala.collection.immutable.Map.empty[Int,scala.collection.immutable.Map[String, Temperature]]) (
        (init,yr) => scala.collection.immutable.Map(yr._1 -> getTemperatureGrid(getTemp(yr._2,_:GridLocation))) , _++_
      )

      println("create broadcast varialble")
      val bcastGridYearlyTemp = spark.sparkContext.broadcast[scala.collection.immutable.Map[Int,scala.collection.immutable.Map[String, Temperature]]](gridYearlyTemp)
      println(new java.util.Date())
      println(gridYearlyTemp.keys.mkString(","))

      println("start making tiles, by year")
      (DEVIATION_START_YEAR to DEVIATION_END_YEAR).foreach { year =>
        println(s"make tiles for ${year} -> ${new java.util.Date()}")

        //par
//          val average = (grid: GridLocation) => bcastAvgGrid.value.get(grid.toString()).get
//          val measured = (grid: GridLocation) => bcastGridYearlyTemp.value.getOrElse(year,Map.empty[String,Temperature]).getOrElse(grid.toString(),0.0)
//          val deviations = (grid: GridLocation) => measured(grid) - average(grid)

        val deviations = (grid: GridLocation) => gridYearlyTemp.getOrElse(year,Map.empty[String,Temperature]).getOrElse(grid.toString(),0.0) - avgGrid.getOrElse(grid.toString(),0.0)

        Visualization2.generateTiles(year,deviations,Visualization.COLOR_TEMPS,hadoopConf,outPrefix)
      }
    }

    case _ => {
      println("making normal tiles")
      val basePath = new Path(s"${inPrefix}target/temperatures/")

      val yearlyTemps = getYearlyTemps(BASELINE_START_YEAR,BASELINE_END_YEAR+1,List.empty[(Int,Iterable[(Location,Temperature)])])
      yearlyTemps.foreach { report =>
        val observations = Manipulation.makeGrid(report._2)
        Visualization2.generateTiles(report._1,observations,Visualization.COLOR_TEMPS,hadoopConf,outPrefix)
      }
    }
  }

  println("done")
  spark.stop

  def getTemp(temps: Iterable[(Location,Temperature)],gridLocation: GridLocation): Temperature = {
    Visualization.predictTemperature(temps, Location(gridLocation.lat.toDouble,gridLocation.lon.toDouble))
  }

  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature = {
    GridLocation => {

      // first make known GridLocation => Temperature
      val knownGrid = makeGrid(temperatures)

      val normalTemperature = normals(GridLocation)
      val knownTemperature = knownGrid(GridLocation)

      knownTemperature - normalTemperature
    }
  }

}
