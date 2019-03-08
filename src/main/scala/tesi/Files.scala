package tesi

import java.util.concurrent.atomic.AtomicInteger

import scala.collection.mutable
import tesi.PrintUtils.print
import tesi.Splitter.split

import scala.io.Source

object Files {
  def readCouplesFile(filePrefix: String)
    : (Matrice[Int], Matrice[PesiType], Matrice[PesiType]) = {
    val suffix = 0.until(8).map(i => ('a' + i).toChar)
    val files = suffix.map(c => Source.fromFile(filePrefix + 'a' + c))
    val filesLines = files.map(_.getLines())
    val numAutori = filesLines(0).next().toInt
    val citatiCreazione: MatriceCreazione[Int] = new Array(numAutori)
    val citazioniCreazione: MatriceCreazione[Short] = new Array(numAutori)
    val coautoriCreazione: MatriceCreazione[Short] = new Array(numAutori)
    val locks = Array.fill(numAutori)(new Object)
    val i = new AtomicInteger(0)
    for (lines <- filesLines.par; line <- lines) {
      val st = split(line, ' ')
      val citatore = st(0).toInt
      val citato = st(1).toInt
      val coautori = st(2).toShort
      val citazioni =
        (st(3).toShort + st(4).toShort + st(5).toShort + st(6).toShort).toShort
      //      val citazioniA = st(4).toShort
      //      val citazioniB = st(5).toShort
      //      val citazioniC = st(6).toShort
      locks(citato).synchronized {
        if (citatiCreazione(citato) == null) {
          citatiCreazione(citato) = mutable.ArrayBuilder.make[Int]
          citazioniCreazione(citato) = mutable.ArrayBuilder.make[Short]
          coautoriCreazione(citato) = mutable.ArrayBuilder.make[Short]
        }
        citatiCreazione(citato) += citatore
        coautoriCreazione(citato) += coautori
        citazioniCreazione(citato) += citazioni
      }

      if (Debug) {
        val num = i.get()
        print(f"Importazione file: ${num * 100.0 / 140376687}%.2f%%\r", num)
        i.incrementAndGet()
      }
    }
    for (file <- files) file.close()

    val citati = new Matrice[Int](numAutori)
    val citazioni = new Matrice[PesiType](numAutori)
    val coautori = new Matrice[PesiType](numAutori)

    for (i <- citati.indices) {
      if (citatiCreazione(i) != null) {
        citati(i) = citatiCreazione(i).result()
        coautori(i) = coautoriCreazione(i).result()
        citazioni(i) = citazioniCreazione(i).result()
        citatiCreazione(i) = null
        coautoriCreazione(i) = null
        citazioniCreazione(i) = null
      }
    }

    (citati, citazioni, coautori)
  }

  def readNumCoautoriFile(fileName: String, numAutori: Int): Array[Short] = {
    val file = Source.fromFile(fileName)
    val res = new Array[Short](numAutori)
    for(line <- file.getLines()) {
      val sp = split(line, ' ')
      res(sp(0).toInt) = sp(1).toShort
    }
    file.close()
    res
  }

  def readPageRankResults(filePrefix: String,
                          numAutori: Int): Matrice[Double] = {
    val filesNames = Array(
      filePrefix + "_normal",
      filePrefix + "_mod",
      filePrefix + "_diff",
      filePrefix + "_diffmod"
    )

    val files = filesNames.map(scala.io.Source.fromFile)
    val results = filesNames.map(_ => new Array[Double](numAutori))

    for {
      i <- files.indices.par
      file = files(i)
      line <- file.getLines()
      sp = split(line, ' ')
      if sp.length == 2
    } results(i)(sp(0).toInt) = sp(1).toDouble

    results
  }
}
