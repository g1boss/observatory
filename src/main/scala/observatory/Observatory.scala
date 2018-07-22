package observatory

import observatory.Main.{args, spark}


class Observatory extends UsingSparkSession {

  def main(args: Array[String]): Unit = {

 //   if(args.length == 0) {
 //     println("you have to specify an file system path")
 //     System.exit(1)
//    }
 //   val fsPrefix = args(0)
    val fsPrefix = if(USING_HDFS) "hdfs://localhost:9000/users/gvb/" else ""

    val BASELINE_START_YEAR = 1975
    val BASELINE_END_YEAR = 1989
    val DEVIATION_START_YEAR = 1991
    val DEVIATION_END_YEAR = 2015

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
        case y if y < maxYear => {
          println(y)
          val tempSeq = extraction.locateTemperatures(year, "/stations.csv", "/" + year + ".csv")
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
          val tempSeq = extraction.locateTemperatures(year, "/stations.csv", "/" + year + ".csv")
          val yearlyDataset = (extraction.locationYearlyAverageRecords(tempSeq))
          getBaselineTemperatures(y + 1, maxYear, yearlyDatasets :+ yearlyDataset)
        }
        case _ => yearlyDatasets
      }

    println(new java.util.Date())

    MODE match {
      case m if m == Mode.Deviations => {
        println("making deviation tiles")
        println(new java.util.Date())

        val yearlyTemps = getBaselineTemperatures(BASELINE_START_YEAR,BASELINE_END_YEAR+1,List.empty[Iterable[(Location,Temperature)]])
        val averages = Manipulation.average(yearlyTemps)

        println("marshalling deviation dataset")
        println(new java.util.Date())
        val observations = getYearlyTemps(DEVIATION_START_YEAR,DEVIATION_END_YEAR+1,List.empty[(Int,Iterable[(Location,Temperature)])])

        println(new java.util.Date())
        observations.foreach { report =>
          val deviations = Manipulation.deviation(report._2, averages)
          Visualization2.generateTiles(report._1,deviations,Visualization2.DEVIATION_COLOR_TEMPS, spark.sparkContext.hadoopConfiguration,fsPrefix)
        }
      }
      case _ => {
        println("making normal tiles")
        println(new java.util.Date())

        val yearlyTemps = getYearlyTemps(BASELINE_START_YEAR,BASELINE_END_YEAR+1,List.empty[(Int,Iterable[(Location,Temperature)])])
        yearlyTemps.foreach { report =>
          val observations = Manipulation.makeGrid(report._2)
          Visualization2.generateTiles(report._1,observations,Visualization.COLOR_TEMPS, spark.sparkContext.hadoopConfiguration, fsPrefix)
        }
      }
    }
    println("done")
    spark.stop

  }
}
