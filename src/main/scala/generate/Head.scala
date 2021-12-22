package generate

val absolute = System.getProperty("user.dir").replaceFirst("mnt/c", "C:")

def generateHead: String =
  val linkTags = config.getCSSFilenames.map(filename => 
    s"<link rel=\"stylesheet\" href=\"$absolute/${config.cssPath}/$filename\">" 
  )
  linkTags.reduce((x, y) => s"$x\n$y")