/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * @author  John Miller, Michael Cotterell
 * @version 1.0
 * @date    Mon Sep 14 14:15:51 EDT 2009
 * @see     LICENSE (MIT style license file).
 */

import java.{ io => jio }
import java.{ util => jutil }

/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * This object is used to build the scalation Scala-based Simulation System.
 * Comment out lines using // for customized builds.  To build the complete
 * scalation system, type the following two commands:
 * > scalac Build.scala
 * > scala Build
 * Check the file called 'errorlog' for errors generated by any of the
 * build functions.  Note, this Build object may be replaced with the
 * Simple Build Tool (sbt) in the future.
 */
object Build extends Application {

	private val base_dir = ""
	private val source_dir = base_dir + "src"
	private val class_dir = base_dir + "classes"
	private val doc_dir = base_dir + "doc"
	private val runsys = Runtime.getRuntime()
	private val errorlog = "errorlog"

	private val scala_home = System.getenv("SCALA_HOME")
	private val scala_bin = scala_home + jio.File.separatorChar + "bin"

	private val os = System.getProperty("os.name").toLowerCase();
	private val is_win = os.indexOf("win") >= 0
	private val is_mac = os.indexOf("mac") >= 0
	private val is_nix = !is_win && !is_mac

	/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	 * Returns the path to the scalac executable
	 */
	private def scalac(): String = {
		var scalac_bin = scala_bin + jio.File.separatorChar + "scalac"
		if (is_win) scalac_bin += ".bat"
		scalac_bin
	}

	/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	 * Returns the path to the scaladoc executable
	 */
	private def scaladoc(): String = {
		var scaladoc_bin = scala_bin + jio.File.separatorChar + "scaladoc"
		if (is_win)	scaladoc_bin += ".bat"
		scaladoc_bin
	}

	/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	 * A utility function to that returns an array that is arr2 appended to arr1
	 */
	private def appendArray[T: ClassManifest](arr1: Array[T], arr2: Array[T]): Array[T] = {
			val result = new Array[T](arr1.length + arr2.length)
			Array.copy(arr1, 0, result, 0, arr1.length)
			Array.copy(arr2, 0, result, arr1.length, arr2.length)
			result
	}

	/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	 * Executes the specified command
	 */
	private def exec(cmd: Array[String]) {
		
		// print out the command
		cmd.foreach(s => print(s + " "))
		println()
		
		// execute the command
		var proc = runsys.exec(cmd, null, null)

		// grab the process's error stream and read it
		var br = new jio.BufferedReader(new jio.InputStreamReader(proc.getErrorStream()))
		var bw = new jio.BufferedWriter(new jio.FileWriter(errorlog))
		var line = br.readLine

		while (line != null) {

			// current timestamp
			var timestamp = new jutil.Date()

			// print to stderr
			System.err.print("[" + timestamp + "] ")
			System.err.println(line)

			// write to errorlog
			bw.write("[" + timestamp + "] ")
			bw.write(line)
			bw.newLine()

			// read the next line
			line = br.readLine()
		}

		// flush and close the writer
		bw.flush()
		bw.close()

		if (proc.waitFor() != 0) {
			throw new Exception("Execution failed")
		}
	}

	/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	 * A utility function that lists all the files with a given extension in p
	 */
	private def listFiles(p: jio.File, ext: String = ""): List[jio.File] = {
		var list = List[jio.File]()
		if (p.isDirectory()) {
			p.listFiles().foreach(f => list = (list ::: listFiles(f, ext)))
		} else {
			if (p.getPath().endsWith(ext)) {
				return List(p)
			}
		}
		return list
	}

	/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	 * Delete a file or directory
	 */
	private def delete(p: jio.File) {
		if (p.exists()) {
			if (p.isDirectory()) p.listFiles.foreach(f => delete(f))
			println("Deleting " + p.getPath())
			p.delete()
		}
	}

	/**
	 * :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	 * Creates a directory
	 */
	private def mkdir(p: jio.File) {
		println("Creating directory " + p.getPath())
		if (!p.exists() || !p.isDirectory()) p.mkdir()
	}

	/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	 * Performs pre-build checks
	 */
	def pre() {
		println("[pre]")
		
		println("OS: " + os)

		// check SCALA_HOME
		if (scala_home == null) {
			throw new Exception("SCALA_HOME environmental variable not set.")
		} else {
			println("SCALA_HOME: " + scala_home)
			println("SCALA_BIN: " + scala_bin)
		}
	}

	/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	 * Cleans up the build environment
	 */
	def clean() {
		println("[clean]")
		
		// delete step
		delete(new jio.File(errorlog))
		delete(new jio.File(doc_dir))
		delete(new jio.File(class_dir))
		
		// create directories if they don't already exist
		mkdir(new jio.File(doc_dir))
		mkdir(new jio.File(class_dir))
	}

	/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	 * Generates the scaladoc documentation
	 */
	def doc() {
		println("[doc]")
		var file_list = listFiles(new jio.File(source_dir), ".scala")
		var cmd = Array(scaladoc,
				"-d", doc_dir,
				"-sourcepath", source_dir,
				"-doc-title", "ScalaTion",
				"-doc-version", "1.0",
				"-encoding", "UTF8")
				file_list.foreach(f => cmd = (appendArray(cmd, Array(f.getPath()))))
				exec(cmd)
	}

	/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	 * Compiles all the packages
	 */
	def compile() {
		// @todo allow for only individual packages to be built
		println("[compile]")
		var file_list = listFiles(new jio.File(source_dir), ".scala")
		var cmd = Array(scalac,
				"-d", class_dir,
				"-sourcepath", source_dir)
				file_list.foreach(f => cmd = (appendArray(cmd, Array(f.getPath()))))
				exec(cmd)
	}

	/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	 * Generate index.html for browsing the source code.
	 */
	def genIndexHtml() {
		println("[genIndexHtml]")
		// @todo generate the index html for source code using GenIndexHtml
	} // genIndexHtml

	/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	 * Perform any post-processing
	 */
	def post() {
		println("[post]")
		// @todo remove colons from generated scaladoc
	}

	// select build functions by moving the comment delimiters (/***, ***/)
	pre()
	clean()
	compile()
	doc()
	genIndexHtml()
	post()

}
