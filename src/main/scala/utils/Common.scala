package utils

import java.io.File

trait Common {

  def curDir: String = {
    val src = s"src${File.separator}main${File.separator}scala"
    val dir = getClass.getName.replaceAll("\\.\\w+\\$$", "").replace('.', File.separatorChar)
    src + File.separator + dir + File.separator
  }

}
