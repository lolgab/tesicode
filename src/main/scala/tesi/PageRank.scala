package tesi

import spire.implicits.cfor
import PrintUtils._

object PageRank {
  def pageRank(citati: Matrice[Int],
                     pesiPageRank: Matrice[Double],
                     iterations: Int,
                     dampingFactor: Double = 0.85): Array[Double] = {
    def pageRankImpl(citati: Matrice[Int],
                           pesiPageRank: Matrice[Double],
                           iterations: Int,
                           dampingFactor: Double = 0.85,
                           sommaPesiUscenti: Array[Double]): Array[Double] = {

      val pageRanksPrevIteration: Array[Double] =
        Array.fill(citati.length)(1.0 / citati.length)
      val pageRanks: Array[Double] =
        Array.fill(citati.length)(0.0)
      val damp: Double = (1.0 - dampingFactor) / citati.length

      cfor(0)(_ < iterations, _ + 1) { iteration =>
        for (i <- citati.indices.par; if citati(i) != null) {
          var pageRankI: Double = 0.0
          val cI = citati(i)
          val cILength = cI.length
          val prI = pesiPageRank(i)
          cfor(0)(_ < cILength, _ + 1) { j =>
            val citatore = cI(j)
            val somma = sommaPesiUscenti(citatore)
            val pesoNormalizzato: Double =
              if (somma == 0.0) 0.0 else prI(j) / somma
            pageRankI += pageRanksPrevIteration(citatore) * pesoNormalizzato
          }
          pageRanks(i) = damp + dampingFactor * pageRankI
        }
        cfor(0)(_ < pageRanks.length, _ + 1) { i =>
          pageRanksPrevIteration(i) = pageRanks(i)
        }
      }
      pageRanks
    }

    val sommaPesiUscenti = Array.fill[Double](citati.length)(0.0)

    cfor(0)(_ < citati.length, _ + 1) { i =>
      if (citati(i) != null) {
        val cI = citati(i)
        val cILength = cI.length
        cfor(0)(_ < cILength, _ + 1) { j =>
          val citatore = cI(j)
          sommaPesiUscenti(citatore) += pesiPageRank(i)(j)
        }
      }
    }

    pageRankImpl(citati,
                       pesiPageRank,
                       iterations,
                       dampingFactor,
                       sommaPesiUscenti)
  }

  def pageRankMod(citati: Matrice[Int],
                  citazioni: Matrice[PesiType],
                  coautori: Matrice[PesiType],
                  sommaCoautori: Array[PesiType],
                  iterazioni: Int,
                  dampingFactor: Double = 0.85): Array[Double] = {
    val numAutori = citati.length
    val sommaCitazioniDate = new Array[PesiType](numAutori)

    cfor(0)(_ < numAutori, _ + 1) { i =>
      if (citati(i) != null) {
        cfor(0)(_ < citati(i).length, _ + 1) { j =>
          val citatore = citati(i)(j)
          sommaCitazioniDate(citatore) =
            (sommaCitazioniDate(citatore) + citazioni(i)(j)).toShort
        }
      }
    }

    val citazioniDateNormalizzate: Matrice[Double] =
      new Matrice[Double](numAutori)
    val citazioniRicevuteNormalizzate: Matrice[Double] =
      new Matrice[Double](numAutori)
    val coautoriNormalizzati: Matrice[Double] =
      new Matrice[Double](numAutori)

    for(i <- citati.indices.par) {
      if (citati(i) != null) {
        citazioniDateNormalizzate(i) = Array.fill[Double](citati(i).length)(0.0)
        citazioniRicevuteNormalizzate(i) = Array.fill[Double](citati(i).length)(0.0)
        coautoriNormalizzati(i) = Array.fill[Double](citati(i).length)(0.0)

        val cI = citati(i)
        val cILength = cI.length
        cfor(0)(_ < cILength, _ + 1) { j =>
          val citatore = cI(j)
          citazioniRicevuteNormalizzate(i)(j) = {
            val n = sommaCitazioniDate(citatore)
            if (n == 0.toShort) 0.0 else citazioni(i)(j).toDouble / n
          }
          citazioniDateNormalizzate(i)(j) = {
            val n = sommaCitazioniDate(i)
            if (n == 0.toShort) 0.0 else {
              val ci = citati(citatore)
              if(ci != null) {
                def met: Double = {
                  cfor(0)(_ < ci.length, _ + 1) { k =>
                    if(ci(k) == i) {
                      return citazioni(citatore)(k).toDouble / n
                    }
                  }
                  0.0
                }
                met
              } else 0.0
            }
          }
          coautoriNormalizzati(i)(j) = {
            val n = sommaCoautori(i)
            if (n == 0.toShort) 0.0 else coautori(i)(j).toDouble / n
          }
        }
      }
    }
    val pesiPageRank = new Matrice[Double](numAutori)
    for (i <- citati.indices.par; if citati(i) != null) {
      pesiPageRank(i) = Array.fill[Double](citati(i).length)(0.0)

      val cI = citati(i)
      val cILength = cI.length
      cfor(0)(_ < cILength, _ + 1) { j =>
        pesiPageRank(i)(j) = citazioniRicevuteNormalizzate(i)(j) * (1 - (coautoriNormalizzati(i)(j) + citazioniDateNormalizzate(i)(j)) / 2.0 )
      }
    }

    pageRank(citati, pesiPageRank, iterazioni, dampingFactor)
  }
}
