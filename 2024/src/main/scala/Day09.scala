object Day09 extends DailyChallenge[Long, Long] {
  def checksum1(diskMap: Vector[Int]): Long = {
    def expand(diskMap: Vector[Int]): Vector[Int] = {
      val expanded = Vector.newBuilder[Int]
      var id = 0
      diskMap.zipWithIndex.foreach { (cnt, idx) =>
        if (idx % 2 == 0) {
          expanded.addAll(Vector.fill(cnt)(id))
          id += 1
        } else {
          expanded.addAll(Vector.fill(cnt)(-1))
        }
      }
      expanded.result()
    }

    def expandedCheckSum(expanded: Vector[Int], curr: Long, l: Int, r: Int): Long =
      if (l > r) curr
      else if (expanded(l) == -1) {
        if (expanded(r) == -1) expandedCheckSum(expanded, curr, l, r - 1)
        else expandedCheckSum(expanded, curr + expanded(r) * l, l + 1, r - 1)
      } else expandedCheckSum(expanded, curr + expanded(l) * l, l + 1, r)

    val expanded = expand(diskMap)
    expandedCheckSum(expanded, 0L, 0, expanded.length - 1)
  }

  def checksum2(diskMap: Vector[Int]): Long = {
    def addIds(diskMap: Vector[Int]): Vector[(Int, Option[Int])] = {
      val withIds = Vector.newBuilder[(Int, Option[Int])]
      var id = 0
      diskMap.zipWithIndex.foreach { (cnt, idx) =>
        if (idx % 2 == 0) {
          withIds.addOne((cnt, Some(id)))
          id += 1
        } else {
          withIds.addOne((cnt, None))
        }
      }
      withIds.result()
    }

    def move(mapWithIds: Vector[(Int, Option[Int])]): Vector[(Int, Option[Int])] = {
      def compress(mapWithIds: Vector[(Int, Option[Int])]): Vector[(Int, Option[Int])] =
        if (mapWithIds.length <= 1) mapWithIds
        else if (mapWithIds(0)._2.isEmpty && mapWithIds(1)._2.isEmpty)
          compress((mapWithIds(0)._1 + mapWithIds(1)._1, None) +: mapWithIds.drop(2))
        else mapWithIds.head +: compress(mapWithIds.tail)

      def moveAux(mapWithIds: Vector[(Int, Option[Int])], id: Int): Vector[(Int, Option[Int])] =
        if (id == -1) mapWithIds
        else {
          val groupIdx = mapWithIds.indexWhere(_._2 == Some(id))
          val group = mapWithIds(groupIdx)
          val fitIdx = mapWithIds.indexWhere((cnt, idOpt) => cnt >= group._1 && idOpt == None)
          if (fitIdx == -1 || fitIdx > groupIdx) moveAux(mapWithIds, id - 1)
          else {
            val fitGroup = mapWithIds(fitIdx)
            val withMove = mapWithIds.updated(fitIdx, group).updated(groupIdx, (group._1, None))
            val withRem =
              if (fitGroup._1 > group._1)
                withMove.take(fitIdx + 1) ++ Vector((fitGroup._1 - group._1, None)) ++ withMove.drop(fitIdx + 1)
              else withMove
            val compressed = compress(withRem)
            moveAux(withRem, id - 1)
          }
        }

      moveAux(mapWithIds, mapWithIds.flatMap(_._2).max)
    }

    def checksum(mapWithIds: Vector[(Int, Option[Int])]): Long = {
      var ans = 0L
      var idx = 0
      mapWithIds.foreach { (cnt, idOpt) =>
        (0 until cnt).foreach { _ =>
          idOpt.foreach(id => ans += id * idx)
          idx += 1
        }
      }
      ans
    }

    checksum(move(addIds(diskMap)))
  }

  def run(input: String): (Long, Long) = {
    val diskMap = input.trim.map(ch => ch - '0').toVector

    val part1 = checksum1(diskMap)
    val part2 = checksum2(diskMap)

    (part1, part2)
  }
}
