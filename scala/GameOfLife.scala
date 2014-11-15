/**
 * Created by asaf david on 14/11/14.
 */
class GameOfLife {

  def neighbors(p : (Int, Int)): List[(Int, Int)] =
    List(
      (p._1 - 1, p._2 - 1),
      (p._1,     p._2 - 1),
      (p._1 + 1, p._2 - 1),
      (p._1 - 1, p._2),
      (p._1 + 1, p._2),
      (p._1 - 1, p._2 + 1),
      (p._1,     p._2 + 1),
      (p._1 + 1, p._2 + 1)
    )

  def neighbors(cells: List[(Int, Int)]): List[(Int, Int)] =
    cells.flatMap(p => neighbors(p)).distinct

  def candidates(cells: List[(Int, Int)]): List[(Int, Int)] =
    (cells ::: neighbors(cells)).distinct

  def applyRules(isCellAlive: Boolean, neighborsAlive: Int): Boolean =
  if (isCellAlive) {
    if (neighborsAlive < 2) false
    else
    if (neighborsAlive == 2 || neighborsAlive == 3) true
    else false
  } else {
    if (neighborsAlive == 3) true else false
  }

  def applyRules(p : (Int, Int), alive: List[(Int, Int)]): Boolean =
    applyRules(
      alive.contains(p),
      neighbors(p).intersect(alive).length)

  def step(cells: List[(Int, Int)]): List[(Int, Int)] =
    candidates(cells).filter(p => applyRules(p, cells))

  def run(cells: List[(Int, Int)], numSteps: Int): List[(Int, Int)] =
    if (numSteps <= 0) cells else run(step(cells), numSteps - 1)

}

object Runner {
  def main(args: Array[String]) {

    val initial = List((0, 0), (-1, 0), (1, 0), (0, -1), (0, 1))

    val game = new GameOfLife

    println(game.run(initial, 10))
  }
}

