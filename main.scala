val filename = "test2"

val dir = "test"
val fileIn = s"$dir/$filename.subst"
val fileOut = s"$dir/out/$filename.html" 

@main def hello = 
    val fileContents = read_markdown(fileIn)
    val parsed = body.parse(fileContents)
    //write(parsed, fileOut)

    val p = formatted.parse("**hey**")
    println(p)