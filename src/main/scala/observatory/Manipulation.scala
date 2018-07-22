package observatory

import java.util.concurrent.ConcurrentHashMap
/**
  * 4th milestone: value-added information
  */
object Manipulation {

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {

    val gridTemps = new ConcurrentHashMap[String,Temperature]

    GridLocation => {
      if (!gridTemps.containsKey(GridLocation.toString())) {
        gridTemps.put(GridLocation.toString(),Visualization.predictTemperature(temperatures,
          Location(GridLocation.lat.toDouble, GridLocation.lon.toDouble)))
      }
      gridTemps.get(GridLocation.toString())
    }
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {

    val gridMakers = new ConcurrentHashMap[Int,GridLocation => Temperature]

    GridLocation => {
      val tempdata = temperaturess.foldLeft(List.empty[Temperature]) {
        (ts,yearlyDataset) =>
          if( !gridMakers.containsKey(yearlyDataset.hashCode()) ) {
            gridMakers.put(yearlyDataset.hashCode(), makeGrid(yearlyDataset))
          }
          val gridMaker = gridMakers.get(yearlyDataset.hashCode())
          ts ::: List(gridMaker(GridLocation))
      }
      tempdata.sum / tempdata.size
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
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

