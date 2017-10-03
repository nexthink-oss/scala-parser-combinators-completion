/*                                                       *\
**  scala-parser-combinators-completion                  **
**  Copyright (c) by Nexthink S.A.                       **
**  Lausanne, Switzerland (http://www.nexthink.com)      **
**  Author: jonas.chapuis@nexthink.com                   **
\*                                                       */

package com.nexthink.utils.parsing.combinator.completion

import org.json4s
import org.json4s.{JArray, JValue}

import scala.util.parsing.input.{NoPosition, Position}
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods._

import scala.collection.immutable

/**  Collection of data types allowing definition of structured parser completions.
  *  A `Completions` instance can contain multiple `CompletionSet`s instances. A `CompletionSet` provides a set of
  *  `Completion` entries and is tagged with a `CompletionTag`.
  *
  *  Sets allow structuring the completion entries into groups, each group tagged with a `label` (plus optional
  *  `description` and `meta`, the latter allowing e.g. encoding visual attributes for the set).
  *  Sets also feature a score, which defines the order between sets within the `Completions` instance.
  *
  *  Each `Completion` entry within a set has a `value`, a `score` and a `meta`:
  *  the score allows ordering the entries within a set, and the meta can e.g. be used to assign a representation style
  *  for a particular completion entry.
  *
  *  Note that specifying tags and sets is optional: if no tag is specified upon creation,
  *  `Completions` instances create a unique default set with an empty tag.
  *
  *  @author Jonas Chapuis
  */
trait CompletionTypes {
  type Elem
  type Elems = Seq[Elem]

  val DefaultCompletionTag   = ""
  val DefaultCompletionScore = 0

  /** Tag defining identification and attributes of a set of completion entries
    * @param label tag label
    * @param score tag score (the higher the better, 0 by default)
    * @param description tag description (optional) - can be used for additional information e.g. for a tooltip
    * @param meta tag meta (optional) - can be used e.g. to define visual style
    */
  case class CompletionTag(label: String, score: Int, description: Option[String], meta: Option[String]) {
    def update(newTag: Option[String], newScore: Option[Int], newDescription: Option[String], newMeta: Option[String]): CompletionTag =
      copy(
        label = newTag.getOrElse(label),
        score = newScore.getOrElse(score),
        description = newDescription.map(Some(_)).getOrElse(description),
        meta = newMeta.map(Some(_)).getOrElse(meta)
      )

    def updateMeta(newMeta: JValue): CompletionTag = copy(meta = Some(encodeJson(newMeta)))

    private[CompletionTypes] def serializeJson: json4s.JObject = {
      ("label" -> label) ~ ("score" -> score) ~ ("description" -> description) ~ ("meta" -> meta)
    }

    override def toString: String = printJson(serializeJson)
    def toJson: JValue            = serializeJson
  }

  case object CompletionTag {
    val Default =
      CompletionTag(DefaultCompletionTag, DefaultCompletionScore, None, None)
    def apply(label: String): CompletionTag =
      CompletionTag(label, DefaultCompletionScore, None, None)
    def apply(label: String, score: Int): CompletionTag =
      CompletionTag(label, score, None, None)
    def apply(label: String, score: Int, description: String): CompletionTag =
      CompletionTag(label, score, Some(description), None)
  }

  /** Set of related completion entries
    * @param tag set tag
    * @param completions set of unique completion entries
    */
  case class CompletionSet(tag: CompletionTag, completions: immutable.HashMap[Elems, Completion]) {
    def label: String                  = tag.label
    def score: Int                     = tag.score
    def description: Option[String]    = tag.description
    def meta: Option[String]           = tag.meta
    def entries: Iterable[Completion]  = completions.values
    def sortedEntries: Seq[Completion] = entries.toSeq.sorted
    def stringEntries: Seq[String]     = sortedEntries.map(_.value.toString)

    private[CompletionTypes] def serializeJson =
      ("tag" -> tag.serializeJson) ~ ("completions" -> entries.map(_.serializeJson).toList)

    override def toString: String = printJson(serializeJson)
    def toJson: JValue            = serializeJson
  }

  case object CompletionSet {
    def apply(tag: CompletionTag, completions: Seq[(Elems, Completion)]): CompletionSet =
      CompletionSet(tag, immutable.HashMap(completions: _*))

    def apply(tag: String, el: Elem): CompletionSet =
      CompletionSet(CompletionTag(tag), Seq(Seq(el) -> Completion(el)))

    def apply(tag: String, elems: Elems): CompletionSet =
      CompletionSet(CompletionTag(tag), Seq(elems -> Completion(elems)))

    def apply(tag: String, completion: Completion): CompletionSet =
      CompletionSet(CompletionTag(tag), Seq(completion.value -> completion))

    def apply(tag: CompletionTag, completions: Iterable[Completion]): CompletionSet =
      CompletionSet(tag, completions.map(c => c.value -> c).toSeq)

    def apply(tag: String, completions: Iterable[Completion]): CompletionSet =
      CompletionSet(CompletionTag(tag), completions.map(c => c.value -> c).toSeq)

    def apply(completions: Iterable[Completion]): CompletionSet =
      CompletionSet(CompletionTag.Default, completions.map(c => c.value -> c).toSeq)

    def apply(completions: Completion*): CompletionSet =
      CompletionSet(CompletionTag.Default, completions.map(c => c.value -> c))

    def apply(el: Elem): CompletionSet =
      CompletionSet(CompletionTag.Default, Seq(Seq(el) -> Completion(el)))

    def apply(completions: Traversable[Elems]): CompletionSet =
      CompletionSet(CompletionTag.Default, completions.map(c => c -> Completion(c)).toSeq)

    implicit def orderingByScoreAndThenAlphabetical: Ordering[CompletionSet] = Ordering.by(s => (-s.score, s.label))
  }

  /** Completion entry
    * @param value entry value (e.g. string literal)
    * @param score entry score (defines the order of entries within a set, the higher the better)
    * @param meta entry meta (e.g. visual style)
    */
  case class Completion(value: Elems, score: Int = DefaultCompletionScore, meta: Option[String] = None) {
    require(value.nonEmpty, "empty completion")
    def updateMeta(newMeta: JValue): Completion = updateMeta(encodeJson(newMeta))
    def updateMeta(newMeta: String): Completion = copy(meta = Some(newMeta))

    private[CompletionTypes] def serializeJson = ("value" -> value.toString()) ~ ("score" -> score) ~ ("meta" -> meta)

    def toJson: String = encodeJson(serializeJson)

    override def toString: String = printJson(serializeJson)
  }
  case object Completion {
    def apply(el: Elem): Completion = Completion(Seq(el))
    implicit def orderingByScoreAndThenAlphabetical: Ordering[Completion] =
      Ordering.by(c => (-c.score, c.value.toString))
  }

  /** Result of parser completion, listing the possible entry alternatives at a certain input position
    * @param position position in the input where completion entries apply
    * @param sets completion entries, grouped per tag
    */
  case class Completions(position: Position, meta: Option[String], sets: immutable.HashMap[String, CompletionSet]) {
    def isEmpty: Boolean                               = sets.isEmpty
    def nonEmpty: Boolean                              = !isEmpty
    def setWithTag(tag: String): Option[CompletionSet] = sets.get(tag)
    def allSets: Iterable[CompletionSet]               = sets.values.toSeq.sorted
    def allCompletions: Iterable[Completion]           = allSets.flatMap(_.sortedEntries)
    def defaultSet: Option[CompletionSet]              = sets.get("")
    def updateMeta(newMeta: JValue): Completions       = updateMeta(encodeJson(newMeta))
    def updateMeta(newMeta: String): Completions       = copy(meta = Some(newMeta))

    private def serializeJson =
      ("position" -> (("line" -> position.line) ~ ("column" -> position.column))) ~ ("meta" -> meta) ~ ("sets" -> allSets.map(_.serializeJson))

    override def toString: String = printJson(serializeJson)
    def toJson: JValue            = serializeJson
    def setsToJson: JArray        = allSets.map(_.serializeJson)

    private def mergeMetaData(left: Option[String], right: Option[String]) = (left, right) match {
      case (Some(l), Some(r)) =>
        (parseOpt(l), parseOpt(r)) match {
          case (Some(lJson), Some(rJson)) => Some(encodeJson(lJson merge rJson))
          case _                          => Some(Seq(l, r).mkString(", "))
        }
      case (Some(l), None) => Some(l)
      case (None, Some(r)) => Some(r)
      case (None, None)    => None
    }

    private def mergeCompletion(left: Completion, right: Completion): Completion = {
      assert(left.value == right.value, "Attempt to merge different completion entries")
      Completion(
        left.value,
        left.score.max(right.score),
        mergeMetaData(left.meta, right.meta)
      )
    }

    private def mergeSets(left: CompletionSet, right: CompletionSet): CompletionSet = {
      assert(left.label == right.label, "Attempt to merge sets with different completion tags")
      CompletionSet(
        CompletionTag(left.tag.label, left.score.max(right.score), left.description.orElse(right.description), mergeMetaData(left.meta, right.meta)),
        left.completions.merged(right.completions)((l, r) => (l._1, mergeCompletion(l._2, r._2)))
      )
    }

    def |(other: Completions): Completions = {
      other match {
        case Completions.empty => this
        case _ =>
          other.position match {
            case otherPos if otherPos < position => this
            case otherPos if otherPos == position =>
              Completions(position, mergeMetaData(meta, other.meta), sets.merged(other.sets)((l, r) => (l._1, mergeSets(l._2, r._2))))
            case _ => other
          }
      }
    }

    def completionStrings: Seq[String] =
      sets.values.toStream
        .sortBy(_.score)
        .reverse
        .flatMap(_.stringEntries)
        .toList

    def takeTop(count: Int): Completions = {
      val allEntries = allSets
        .flatMap(s => s.completions.values.map((_, s.tag)))
        .toList
      val sortedEntries =
        allEntries
          .sortBy {
            case (Completion(_, score, _), CompletionTag(_, tagScore, _, _)) =>
              (tagScore, score)
          }
          .reverse
          .take(count)
      val regroupedSets = sortedEntries
        .groupBy { case (_, tag) => tag }
        .map {
          case (groupTag, completions) =>
            CompletionSet(groupTag, completions.map(c => c._1))
        }
      Completions(position, meta, regroupedSets.map(s => s.tag.label -> s).toSeq)
    }

    def setsScoredWithMaxCompletion(): Completions = {
      Completions(position, meta, sets.mapValues(s => CompletionSet(s.tag.copy(score = s.completions.values.map(_.score).max), s.completions)).toSeq)
    }
  }

  private def encodeJson(meta: JValue) = compact(render(meta))
  private def printJson(meta: JValue)  = pretty(render(meta))

  case object Completions {
    def apply(position: Position, meta: Option[String], completionSets: Seq[(String, CompletionSet)]): Completions =
      Completions(position, meta, immutable.HashMap(completionSets: _*))
    def apply(position: Position, completionSet: CompletionSet): Completions =
      Completions(position, None, Seq(completionSet.tag.label -> completionSet))
    def apply(position: Position, meta: Option[String], completionSet: CompletionSet): Completions =
      Completions(position, None, Seq(completionSet.tag.label -> completionSet))
    def apply(position: Position, meta: Option[String], completions: Traversable[Elems]): Completions =
      Completions(position, meta, CompletionSet(completions))
    def apply(position: Position, completions: Traversable[Elems]): Completions =
      Completions(position, None, CompletionSet(completions))
    def apply(position: Position, meta: Option[String], completionSets: Iterable[CompletionSet]): Completions =
      Completions(position, meta, completionSets.map(s => s.tag.label -> s).toSeq)
    def apply(position: Position, completionSets: Iterable[CompletionSet]): Completions =
      Completions(position, None, completionSets.map(s => s.tag.label -> s).toSeq)
    def apply(completionSet: CompletionSet): Completions =
      Completions(NoPosition, None, completionSet)
    def apply(completionSets: Iterable[CompletionSet]): Completions =
      Completions(NoPosition, None, completionSets.map(s => s.tag.label -> s).toSeq)

    val empty = Completions(NoPosition, None, immutable.HashMap[String, CompletionSet]())
  }

}
