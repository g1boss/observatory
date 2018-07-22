package observatory

import org.apache.spark.sql.SparkSession
import org.apache.spark.SparkConf

trait UsingSparkSession {

  @transient final lazy val spark: SparkSession = SparkSession.builder()
    .config("spark.app.name", "Observatory")
    .master("local[5]")
    .config("spark.driver.memory", "4G")
    .config("num.executors", "2")
    .config("executor.cores", "2")
    .config("spark.executor.memory", "4G")
    .getOrCreate()


  var USING_HDFS: Boolean = true
  final val hadoopConf = spark.sparkContext.hadoopConfiguration

  def stop(): Unit = {
    spark.stop()
  }
}

object UsingSparkSession {

}
