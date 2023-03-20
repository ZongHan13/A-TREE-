name := "KafkaTest"
organization := "com.pcdm.han"
version := "1.0-SNAPSHOT"
scalaVersion := "2.13.8"

libraryDependencies ++= Seq(
                            "org.apache.spark" %% "spark-core" % "3.2.0",
                            "org.apache.spark" %% "spark-streaming" % "3.2.0" ,
                            "org.apache.spark" %% "spark-streaming-kafka-0-10" % "3.2.0",
                            "org.apache.spark" %% "spark-sql" % "3.2.0",
                            "org.apache.kafka" %% "kafka" % "3.1.0",
                            "org.apache.spark" %% "spark-sql-kafka-0-10" % "3.2.0",
                            "com.lihaoyi" %% "os-lib" % "0.9.0",
                            "com.typesafe.akka" %% "akka-actor" % "2.7.0"
)