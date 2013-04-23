package gpp.util
import gpp.util._

object ConvertStanford {

  def main(args: Array[String]) {
    val stanfordData = getData(args.mkString)
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
