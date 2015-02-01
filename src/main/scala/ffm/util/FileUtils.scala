package ffm.util

import java.io.File
import java.io.PrintWriter

object FileUtils {
  
  def withPrintWriter(path: String)(f: PrintWriter => Unit): Unit =
    withPrintWriter(new File(path))(f)
  
  def withPrintWriter(file: File)(f: PrintWriter => Unit): Unit = {
    val writer = new PrintWriter(file)
    try {
      f(writer)
    } finally {
      writer.close()
    }
  }
  
  def fileName(path: String): Option[String] = {
    val tpath = path.trim()
    
    if (tpath.isEmpty()) None
    else {
      val f = new File(tpath)
      if (f.isFile()) Some(f.getName())
      else None
    }
  }
  
  def removeExtension(name: String): String = {
    val i = name.lastIndexOf(".")
    if (i > 0) name.substring(0, i)
    else name
  }
  
  def makePath(dir: String, fname: String): String = {
    if (dir.isEmpty()) fname
    else if (dir.endsWith("/")) dir + fname
    else dir + "/" + fname
  }
}