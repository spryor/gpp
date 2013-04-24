package gpp.app

import gpp.util._

object Convert_Stanford {

  def main(args: Array[String]) {
    if(args.length < 1) {
      println("Error: File path missing")
      System.exit(1)
    }
 
    val stanfordData = getData(args(0).mkString)
    val marshalled =
      <dataset>
      { stanfordData.map { data =>
        <item label={getPolarity(data._1)}>
          <content>{data._2}</content>
        </item>
      }}
      </dataset>

    println(marshalled.mkString)
  }

  def getData(filename:String) = {
    io.Source.fromFile(filename)
      .getLines
      .map(line => {
          val tokens = line.split(";;")
          (tokens(0),tokens(tokens.length-1))
          })
      .toArray
  }

  def getPolarity(num:String) = {
    num match {
      case "0" => "negative"
      case "2" => "neutral"
      case _ => "positive"
    }
  }
}

object Convert_Emoticon {

  def main(args: Array[String]) {
    val files = IndexedSeq("happy.txt","neutral.txt","sad.txt")
    val dataDir = args(0).mkString + "/"
    val twitterData = getData(dataDir+files(0)) ++ getData(dataDir+files(1)) ++ getData(dataDir+files(2)) 
    val marshalled = 
      <dataset>
      { twitterData.map { data =>
        <item label={data._1}>
          <content>{data._2}</content>
        </item>
      }}
      </dataset>
    println(marshalled.mkString)
  }

  def getData(filename:String) = {
    val sentiment = getSentiment(filename)
    val output = io.Source.fromFile(filename)
      .getLines
      .map(line => {
        val text = line.split("\t")(2).mkString
        (sentiment, text)
        })
      .toSet
    output
  }

  def getSentiment(filename:String):String = {
    filename match {
      case f if f.endsWith("happy.txt") => "positive"
      case f if f.endsWith("sad.txt") => "negative"
      case _ => "neutral"
    }
  }
}
