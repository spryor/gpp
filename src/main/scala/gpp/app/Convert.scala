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

