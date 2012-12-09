package ss2js.test

import java.io.FileReader
import javax.script.ScriptEngineManager

object RunJS {

  def main(args: Array[String]) {
	  run(args)
  }

  def run(files: Traversable[String]) {
    val manager = new ScriptEngineManager()
    val engine = manager.getEngineByExtension("js")
    files.foreach { file =>
      println("run " + file)
      val reader = new FileReader(file)
      engine.eval(reader)
      reader.close()
    }
    println("OK")
  }

}