package com.sambeth.algoexpert.easy

import scala.collection.mutable
import scala.collection.immutable

object TournamentWinner {

  private def updatedScores(scores: mutable.Map[String, Int], team: String, points: Int): Unit =
    if scores.contains(team) then
      scores.put(team, scores(team) + points)
    else
      scores.put(team, points)

  private def tournamentWinnerOne(competitions: List[List[String]], results: List[Int]): String =
    val scores: mutable.Map[String, Int] = mutable.Map.empty[String, Int]

    for
      idx <- results.indices
    do
      if results(idx) == 0 then
        if scores.contains(competitions(idx)(1)) then
          scores.put(competitions(idx)(1), scores(competitions(idx)(1)) + 3)
        else
          scores.put(competitions(idx)(1), 3)

      else
        if scores.contains(competitions(idx).head) then
          scores.put(competitions(idx).head, scores(competitions(idx).head) + 3)
        else
          scores.put(competitions(idx).head, 3)

    (scores.max)._1


  private def tournamentWinnerTwo(competitions: List[List[String]], results: List[Int]): String =
    var bestTeam: String = "None"
    val scores: mutable.Map[String, Int] = mutable.Map[String, Int](bestTeam -> 0)

    for
      (competition, idx) <- competitions.zipWithIndex
    do
      var result = results(idx)
      var homeTeam :: awayTeam :: Nil = competition

      var winningTeam: String = if (result == 0) awayTeam else homeTeam
      updatedScores(scores, winningTeam, 3)

      if scores(winningTeam) > scores(bestTeam) then
        bestTeam = winningTeam

    bestTeam

  private def tournamentWinnerRecursion(competitions: List[List[String]], results: List[Int]): String =
    // val winningTeamScore: immutable.Map[String, Int] = immutable.Map.empty[String, Int]
    ???

  @main def mainFour: Unit =
    println(tournamentWinnerOne(
      List(
        List("HTML", "C%"),
        List("C%", "Python"),
        List("Python", "HTML"),
      ),
      List(0, 0, 1)
    ))

    println(tournamentWinnerTwo(
      List(
        List("HTML", "C%"),
        List("C%", "Python"),
        List("Python", "HTML"),
      ),
      List(0, 0, 1)
    ))

//    println(tournamentWinnerRecursion(
//      List(
//        List("HTML", "C%"),
//        List("C%", "Python"),
//        List("Python", "HTML"),
//      ),
//      List(0, 0, 1)
//    ))

}
