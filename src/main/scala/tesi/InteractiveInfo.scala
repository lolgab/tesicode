package tesi

import scala.io.StdIn

object InteractiveInfo {
  def main(args: Array[String]): Unit = {
    if(args.length != 2) {
      println("Il programma richiede due parametri, file relazioni e file pageranks.")
      System.exit(1)
    }
    val pageranks = Files.readPageRankResults(args(1), 1766461)
    val (citati, citazioni, coautori) = Files.readCouplesFile(args(0))

    while(true) {
      try {
        print("Scrivi un id autore, -1 per uscire > ")
        var auth: Int = -1
        val line  = StdIn.readLine().trim
        try {
           auth = line.toInt
        } catch {
          case e: NumberFormatException =>
            println(s"$line non è un numero valido")
        }

        if(auth == -1) {
          println("Bye!")
          System.exit(0)
        }

        def volte(v: Int) = if(v == 1) s"$v volta" else s"$v volte"

        val c = citati(auth)
        if(c != null) {
          for(j <- c.indices) {
            val cits = volte(citazioni(auth)(j))
            val coau = volte(coautori(auth)(j))
            println(s"${c(j)} cita $auth $cits e sono coautori $coau")
          }
        }
        for(i <- citati.indices; if citati(i) != null; j <- citati(i).indices; citatore = citati(i)(j); if citatore == auth) {
          val cits = volte(citazioni(i)(j))
          val coau = volte(coautori(i)(j))
          println(s"$i è citato da $auth $cits e sono coautori $coau")
        }
        println(s"$auth ha PageRank pari a ${pageranks(0)(auth)}")
        println(s"$auth ha PageRank modificato pari a ${pageranks(1)(auth)}")
        println(s"$auth ha PageRank differenza logaritmica pari a ${pageranks(2)(auth)}")
        println(s"$auth ha PageRank differenza logaritmica in valore assoluto pari a ${pageranks(3)(auth)}")
      } catch {
        case e: Throwable => e.printStackTrace()
      }
    }
  }
}
