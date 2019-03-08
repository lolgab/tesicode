package tesi

import java.io._

import tesi.TimeUtils._

object PageRankEIJMain {
  def main(args: Array[String]): Unit = {
    val nomi = if(args.length == 3) args else Array("outFile", "numCoautori", "prout") 
    var (citati, citazioni, coautori) =
      timeInfo("Read file")(Files.readCouplesFile(nomi(0)))
    val pageRanksNormale = timeInfo("Page Rank normale") {
      val pesiPageRank = new Matrice[Double](citati.length)
      for (i <- pesiPageRank.indices.par) {
        if (citati(i) != null) pesiPageRank(i) = citati(i).map(_.toDouble)
      }
      PageRank.pageRank(citati, pesiPageRank, 18)
    }
    val numCoautori = Files.readNumCoautoriFile(nomi(1), citati.length)
    val pageRanksMod = timeInfo("Page Rank modificato") {
      PageRank.pageRankMod(citati, citazioni, coautori, numCoautori, 18)
    }
    citati = null
    citazioni = null
    coautori = null

    var outFile =
      new BufferedWriter(new FileWriter(new File(nomi(2) + "_mod")), 0x100000)
    for ((score, i) <- pageRanksMod.zipWithIndex.sortBy(_._1))
      outFile.write(s"$i $score\n")

    outFile = new BufferedWriter(new FileWriter(new File(nomi(2) + "_normal")),
                                 0x100000)
    for ((score, i) <- pageRanksNormale.zipWithIndex.sortBy(_._1))
      outFile.write(s"$i $score\n")
    outFile.close()

    outFile =
      new BufferedWriter(new FileWriter(new File(nomi(2) + "_diff")), 0x100000)
    var logaritms = new Array[Double](pageRanksNormale.length)
    for (i <- pageRanksNormale.indices) {
      logaritms(i) =
        Math.log(pageRanksNormale(i)) - Math.log(pageRanksMod(i))
    }
    for ((score, i) <- logaritms.zipWithIndex.sortBy(_._1))
      outFile.write(s"$i $score\n")
    outFile.close()

    outFile = new BufferedWriter(new FileWriter(new File(nomi(2) + "_diffmod")),
                                 0x100000)
    logaritms = new Array[Double](pageRanksNormale.length)
    for (i <- pageRanksNormale.indices)
      logaritms(i) =
        Math.abs(Math.log(pageRanksNormale(i)) - Math.log(pageRanksMod(i)))

    for ((score, i) <- logaritms.zipWithIndex.sortBy(_._1))
      outFile.write(s"$i $score\n")
    outFile.close()
  }
}
