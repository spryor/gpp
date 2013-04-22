package gpp.app

object Exp{
 
  import gpp.util.English
  import java.io.File
  import nak.util.ConfusionMatrix
  import nak.NakContext._
  import nak.core._
  import nak.data._
  import nak.liblinear._  
  import nak.util.ConfusionMatrix
  import chalk.lang.eng.Twokenize  
 
  def main(args: Array[String]){
    val opts = ExpOpts(args)
    
    opts.method() match {
      case "majority" => val (goldLabels, predictedLabels, items) = majorityMethod(opts.train(), opts.eval())
                        println(ConfusionMatrix(goldLabels, predictedLabels, items))
      case "lexicon" => val (goldLabels, predictedLabels, items) = lexiconMethod(opts.eval())
                        println(ConfusionMatrix(goldLabels, predictedLabels, items))
      case "L2R_LR" => val (goldLabels, predictedLabels, items) = l2rlrMethod(opts.train(),opts.eval(),opts.cost().toDouble, opts.extended())
                        println(ConfusionMatrix(goldLabels, predictedLabels, items))
      case _ => println("Method not implemented")
    }
  }

  /**
   * A function to use the lexicon method for SA.
   * 
   * @param trainFile - the path to the training xml file
   * @param evalFile - the path to the evaluation xml file
   */
  def majorityMethod(trainFile:String, evalFile:String) = {
    val trainData = readXML(trainFile)
    val evalData = readXML(evalFile)
   
    val topLabel = (trainData \ "item")
                     .map(item => (item \ "@label").text)
                     .groupBy(l => l).mapValues(_.length)
                     .toList
                     .reduceLeft((a, b) => if(a._2 > b._2) a else b)
                     ._1
    
    val goldLabels = (evalData \ "item").map{item => (item \ "@label").text}
    val predictedLabels = goldLabels.map(_ => topLabel)
    (goldLabels, predictedLabels, goldLabels)
  }
 
  def lexiconMethod(evalFile:String) = {
    val evalData = readXML(evalFile)
    val goldLabels = (evalData \ "item").map{item => (item \ "@label").text}
    val predictedLabels = (evalData \ "item")
                            .map{item => 
                              {
                              val labelAssignment = Twokenize(item.text)
                                                .map(English.polarityLexicon)
                                                .sum
                              if(labelAssignment > 0) {
                                "positive"
                              } else if(labelAssignment < 0) {
                                "negative"
                              } else {
                                "neutral"
                              }
                            }
                          }
    (goldLabels, predictedLabels, goldLabels)
  }

  def l2rlrMethod(trainFile:String, 
                  evalFile:String, 
                  costParam:Double, 
                  extended:Boolean):(Seq[String], Seq[String], Seq[String]) = {

    val cost = costParam

    def readRaw(filename: String) = {
      for(item <- (readXML(filename) \ "item")) 
        yield Example((item \ "@label").text, item.text.trim)
    }
    
    val simpleFeaturizer = new Featurizer[String, String] {
      def apply(input: String) = input
       .replaceAll("""([\?!\";\|\[\]])""", " $1 ") 
       .trim
       .split("\\s+")
       .map(tok => FeatureObservation("word="+tok))
    }

    val extendedFeaturizer = new BowFeaturizer

    val rawExamples = readRaw(trainFile)

    val config = LiblinearConfig(cost=cost)
     
    val classifier = trainClassifier(config, 
                                     if(extended) extendedFeaturizer else simpleFeaturizer,
                                     rawExamples)

    def maxLabelPpa = maxLabel(classifier.labels) _

    val comparisons = for (ex <- readRaw(evalFile).toList) yield 
      (ex.label, maxLabelPpa(classifier.evalRaw(ex.features)), ex.features)

    comparisons.unzip3
 }


  /**
    * A simple helper function for reading XML files
    *
    * @param path - The path to the XML file to be read
    */
  def readXML(path:String) = scala.xml.XML.loadFile(path)

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
    
    val methodTypes = Set("lexicon", "L2R_LR", "majority")
    val train = opt[String]("train", short='t', descr="The files containing training events.")
    val eval = opt[String]("eval", short='e', required=true, descr="The files containing evalualation events.")
    val method = opt[String]("method", short='m', default=Some("L2R_LR"), descr="The type of solver to use.")
    val cost = opt[String]("cost", short='c', default=Some("1.0"), descr="Cost parameter for the L2R_LR method.")
    val extended = opt[Boolean]("extended", short='x', default=Some(false), descr="Use extended features")
  }
}


