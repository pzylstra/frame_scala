package ffm.runner

object CSVRunner {
  case class CmdLineArgs(paramFile: String, dbFile: String, dbRecreate: Boolean) 
  
  def main(args: Array[String]) {
    val runparams = parseArgs(args)
    
    println(s"param file: ${runparams.paramFile}")
    println(s"db file: ${runparams.dbFile}")
    println(s"db recreate: ${runparams.dbRecreate}")
  }
  
  def parseArgs(args: Array[String]): CmdLineArgs = {
    val params = args(args.indexOf("-p") + 1)
    val db = args(args.indexOf("-d") + 1)
    val dbRecreate = args.indexOf("-x") != -1
    
    CmdLineArgs(params, db, dbRecreate)
  }
}