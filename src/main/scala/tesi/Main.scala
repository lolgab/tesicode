package tesi

import java.io.{BufferedReader, BufferedWriter, FileReader, FileWriter}

import spire.implicits.cfor
import tesi.BinarySearch.binarySearch
import tesi.PrintUtils._
import tesi.Splitter._

import scala.collection.mutable

class Line(val id: Int, val references: Array[Int], val authors: Array[String])

object Main {
  def readLines(f: String): (Array[Line], Int, Int) = {
    val reader = new BufferedReader(new FileReader(f))
    val numArticoli = reader.readLine().toInt
    val arr = new Array[Line](numArticoli)
    var i = 0
    var line = reader.readLine()
    while (line != null) {
      try {
        val sp = split(line, ',')
        val id = sp(0).toInt
        val refs = {
          val buf = split(sp(1), ';')
          val res = new Array[Int](buf.length)
          cfor(0)(_ < buf.length, _ + 1) { i =>
            res(i) = buf(i).toInt
          }
          res
        }
        val auths = split(sp(2), ';').toArray
        arr(i) = new Line(id, refs, auths)
        if (Debug) print(s"Reading file: ${i}th line.\r", i)
        i += 1
      } catch { case e: Throwable => e.printStackTrace() }
      line = reader.readLine()
    }
    (arr, numArticoli, i)
  }

  @inline def incrementValueIn(l: mutable.ArrayBuffer[Array[Int]],
                               author: Int,
                               relation: Int): Unit = {
    var found = false
    cfor(0)(!found && _ < l.length, _ + 1) { k =>
      if (l(k)(0) == author) {
        l(k)(relation) += 1
        found = true
      }
    }
    if (!found) {
      val arr = new Array[Int](ArrayRelazioneLength)
      arr(Autore) = author
      arr(relation) = 1
      l += arr
    }
  }

  @inline def incrementValueInIfExists(l: mutable.ArrayBuffer[Array[Int]],
                                       author: Int,
                                       relation: Int): Unit = {
    var found = false
    cfor(0)(!found && _ < l.length, _ + 1) { k =>
      if (l(k)(0) == author) {
        l(k)(relation) += 1
        found = true
      }
    }
  }

  def main(args: Array[String]): Unit = {
    if (args.length != 3) System.exit(1)
    val (file, numArticoli, numLines) = readLines(args(0))
    var authorsTreeSet = new mutable.TreeSet[String]()
    cfor(0)(_ < numLines, _ + 1) { index =>
      val line = file(index)
      for (a <- line.authors) {
        authorsTreeSet += a
      }
      if (Debug)
        print(
          f"Creating authors TreeSet ${100.0 * index / numLines}%.2f%% of dataset.\r",
          index)
    }
    if (Debug) println()
    var authors: Array[String] = authorsTreeSet.toArray
    authorsTreeSet = null
    var authorsDisambiguati: Array[Int] = authors.indices.toArray

    val relazioni: Array[mutable.ArrayBuffer[Array[Int]]] =
      Array.fill(authors.length)(mutable.ArrayBuffer.empty)

    var autoriArticolo = new Array[Array[Int]](numArticoli)
    cfor(0)(_ < numLines, _ + 1) { index =>
      val line = file(index)
      for (ref <- line.references)
        if (autoriArticolo(ref) == null)
          autoriArticolo(ref) = Array.empty
      val authsIdx: Array[Int] =
        line.authors.map(a => authorsDisambiguati(binarySearch(authors, a)))
      autoriArticolo(line.id) = authsIdx
      if (Debug)
        print(
          f"Creating and articleToAuthors map ${100.0 * index / numLines}%.2f%% of dataset.\r",
          index)
    }
    if (Debug) println("\nCreata mappa articolo -> autori")

    val authorsDisambiguatiDistinct = authorsDisambiguati.distinct
    authorsDisambiguati = null
    authors = null

    cfor(0)(_ < numLines, _ + 1) { index =>
      val line = file(index)
      for (ref <- line.references; citer <- autoriArticolo(line.id);
           cited <- autoriArticolo(ref)) {
        val tipo =
          if (cited != citer) {
            val isCitaComeCoautoreA = autoriArticolo(ref).contains(citer)
            val isCitaComeCoautoreB = autoriArticolo(line.id).contains(cited)

            if (isCitaComeCoautoreA && isCitaComeCoautoreB)
              CitaComeCoautoreAAndB
            else if (isCitaComeCoautoreA)
              CitaComeCoautoreA
            else if (isCitaComeCoautoreB)
              CitaComeCoautoreB
            else
              Cita
          } else Cita

        incrementValueIn(relazioni(citer), cited, tipo)
      }
      if (Debug)
        print(
          f"Creating cites authors matrix ${100.0 * index / numLines}%.2f%% of dataset.\r",
          index)
    }
    if (Debug) println()
    val numCoautori = new Array[Short](relazioni.length)
    cfor(0)(_ < numLines, _ + 1) { index =>
      val line = file(index)
      cfor(0)(_ < line.authors.length, _ + 1) { i =>
        cfor(0)(_ < line.authors.length, _ + 1) { j =>
          if (j != i) {
            val authsIdx = autoriArticolo(line.id)
            val iId = authsIdx(i)
            val jId = authsIdx(j)

            numCoautori(iId) = (numCoautori(iId) + 1.toShort).toShort
            incrementValueInIfExists(relazioni(iId), jId, Coautore)
          }
        }
      }
      if (Debug)
        print(
          f"Adding coauthor information ${100.0 * index / numLines}%.2f%% of dataset.\r",
          index)
    }
    if (Debug) println()
    autoriArticolo = null

    val outFile =
      new BufferedWriter(new FileWriter(new java.io.File(args(1))))
    outFile.write(s"${authorsDisambiguatiDistinct.length}\n")

    val numCoautoriFile =
      new BufferedWriter(new FileWriter(new java.io.File(args(2))))

    cfor(0)(_ < authorsDisambiguatiDistinct.length, _ + 1) { index =>
      val author = authorsDisambiguatiDistinct(index)
      numCoautoriFile.write(s"$author ${numCoautori(author)}\n")
      for (rel <- relazioni(author))
        outFile.write(s"$author ${rel.mkString(" ")}\n")
      if (Debug)
        print(
          f"Writing authors couple list ${100.0 * index / authorsDisambiguatiDistinct.length}%.2f%% of dataset.\r",
          index)
    }
    outFile.close()
    if (Debug) println()
  }
}
