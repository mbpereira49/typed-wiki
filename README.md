# typed-wiki

## Installation

You can run the code if you have Scala 3 and sbt installed. 

Clone the repository, navigate to the root, and run `sbt`. Once the sbt server is running, run the command `run <filename>.dom <filename>.subst` to test the program. Until I get the chance to make it more user-friendly, you can only specify one domain file and one substance file (in that order) which exist in the directory `src/main/scala/test`. The output file should reside within `src/main/scala/test/out`.

With the pre-existing files in the test directory, the command that should work is `run simple.dom simple.subst`.