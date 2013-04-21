package gpp.app

object Exp{
 
  import java.io.File
  import nak.util.ConfusionMatrix
 
  def main(args: Array[String]){
    val opts = ExpOpts(args)
    val trainData = scala.xml.XML.loadFile(opts.train())
    val evalData = scala.xml.XML.loadFile(opts.eval())
   
    val topLabel = (trainData \ "item")
                        .map(item => (item \ "@label").text)
                        .groupBy(l => l).mapValues(_.length)
                        .toList
                        .reduceLeft((a, b) => if(a._2 > b._2) a else b)
                        ._1
    
    val goldLabels = (evalData \ "item").map{item => (item \ "@label").text}
    val predictedLabels = goldLabels.map(_ => topLabel)

    println(ConfusionMatrix(goldLabels, predictedLabels, goldLabels))

  }
}

object ExpOpts {
  import org.rogach.scallop._

  def apply(args: Array[String]) = new ScallopConf(args){
    banner("""
Classification application.

For usage see below:
       
  -c, --cost  <arg>       The cost parameter C. Bigger values means less
                          regularization (more fidelity to the training set).
                          (default = 1.0)
  -d, --detailed          
  -e, --eval  <arg>...    The files containing evalualation events.
  -x, --extended          Use extended features.
  -m, --method  <arg>     The type of solver to use. Possible values: majority,
                          lexicon, or any liblinear solver type.
                          (default = L2R_LR)
  -t, --train  <arg>...   The files containing training events.
  -v, --verbose           
      --help              Show this message
      --version           Show version of this program
                       """)

    val train = opt[String]("train", short='t', required=true, descr="The files containing training events.")
    val eval = opt[String]("eval", short='e', required=true, descr="The files containing evalualation events.")
    val method = opt[String]("method", short='m', default=Some("L2R_LR"), descr="The type of solver to use.")
  }
}


