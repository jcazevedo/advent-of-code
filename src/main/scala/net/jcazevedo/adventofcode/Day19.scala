package net.jcazevedo.adventofcode

class Day19 extends DailyChallenge[Int, Int] {
  case class Elf(index: Int) {
    var next: Elf = null
    var deleted: Boolean = false
    def setNext(elf: Elf): Unit = this.next = elf
    def delete: Unit = this.deleted = true
  }

  def run(filename: String): (Int, Int) = {
    val nElves = io.Source.fromFile(filename).getLines.mkString.toInt
    val elves = (1 to nElves).map(Elf(_)).toIndexedSeq
    (0 until elves.length).foreach { idx =>
      if (idx + 1 < elves.length)
        elves(idx).setNext(elves(idx + 1))
      else
        elves(idx).setNext(elves(0))
    }
    var curr = elves(0)
    while (curr.next != curr) {
      curr.setNext(curr.next.next)
      curr = curr.next
    }
    var remElves = nElves
    var elfToRemove = 0
    var elfRemoved = nElves / 2
    while (remElves > 1) {
      elves(elfRemoved).delete
      do {
        elfToRemove = (elfToRemove + 1) % nElves
      } while (elves(elfToRemove).deleted)
      do {
        elfRemoved = (elfRemoved + 1) % nElves
      } while (elves(elfRemoved).deleted)
      if (remElves % 2 != 0) {
        do {
          elfRemoved = (elfRemoved + 1) % nElves
        } while (elves(elfRemoved).deleted)
      }
      remElves -= 1
    }
    (curr.index, elves(elfToRemove).index)
  }
}
