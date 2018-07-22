package observatory

import org.apache.spark.sql.SparkSession
import org.junit.runner.RunWith
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll, FunSuite}
import org.scalatest.junit.JUnitRunner

trait ExtractionTest extends FunSuite  with BeforeAndAfterAll {

  val testObject = Extraction

  override def beforeAll = Extraction.USING_HDFS = false

  override def afterAll = Extraction.spark.stop

  test("testObject can be instantiated") {
    val instantiatable = try {
      testObject.hashCode()
      true
    } catch {
      case _: Throwable => false
    }
    assert(instantiatable, "Can't instantiate an Extraction object")
  }

  test("try to read some annual station data") {
    val temps = testObject.locateTemperatures(2015, "/teststations.csv", "/2015.csv")
    val n = temps.size
    assert(n == 365 , "not the right number of temps")
  }

  test("average station data") {
    val temps = testObject.locateTemperatures(2015, "/teststations.csv", "/2015.csv")
    val avgtemps = testObject.locationYearlyAverageRecords(temps)
    val n = avgtemps.size
    assert(n == 1, "not the right number of average temps")
  }

}