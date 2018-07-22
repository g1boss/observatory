/*
package observatory


import org.apache.spark.SparkConf
import org.apache.spark.sql.SparkSession

trait SparkSessionSetup {
  def withSparkSession(testMethod: (SparkSession) => Any) {
    val conf = new SparkConf()
      .setMaster("local")
      .setAppName("Spark test")
    val spark = new SparkSession(conf)
    try {
      testMethod(spark)
    }
    finally spark.stop()
  }
}
*/