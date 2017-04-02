import Math.min
val width = 5
val numTasks = 4
val height = 15
val stripHeight = height / numTasks
val startPoints = for {task <- 0 until numTasks} yield task * stripHeight
val strips = startPoints.zipWithIndex.map {
  case (from, i) if i < numTasks - 1 => (from, from + stripHeight)
  case (from, i) => (from, height)
}
strips.size
