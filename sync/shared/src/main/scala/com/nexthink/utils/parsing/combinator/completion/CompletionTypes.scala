package com.nexthink.utils.parsing.combinator.completion

import io.circe.syntax._
import io.circe.Json
import io.circe.generic.auto._
import scala.util.parsing.input.{NoPosition, Position}
import com.nexthink.utils.parsing.collections.SortingHelpers.lazyQuicksort

import scala.collection.immutable
import scala.language.implicitConversions

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
  case class CompletionTag[M](label: String, score: Int, description: Option[String], meta: Option[M]) {
    def update(newTag: Option[String], newScore: Option[Int], newDescription: Option[String], newMeta: Option[M]): CompletionTag[M] =
      copy(
        label = newTag.getOrElse(label),
        score = newScore.getOrElse(score),
        description = newDescription.map(Some(_)).getOrElse(description),
        meta = newMeta.map(Some(_)).getOrElse(meta)
      )
    def withMeta[M](newMeta: M): CompletionTag[M] = copy(meta = Some(newMeta))

    override def toString: String = this.asJson.spaces2
    def toJson: Json              = this.asJson
  }

  case object CompletionTag {
    val Default =
      CompletionTag(DefaultCompletionTag, DefaultCompletionScore, None, None)
    def apply(label: String): CompletionTag[Nothing] =
      CompletionTag(label, DefaultCompletionScore, None, None)
    def apply[M](label: String, meta: M): CompletionTag[M] =
      CompletionTag(label, DefaultCompletionScore, None, Some(meta))
    def apply(label: String, score: Int): CompletionTag[Nothing] =
      CompletionTag(label, score, None, None)
    def apply[M](label: String, score: Int, meta: M): CompletionTag[M] =
      CompletionTag(label, score, None, Some(meta))
    def apply(label: String, score: Int, description: String): CompletionTag[Nothing] =
      CompletionTag(label, score, Some(description), None)
  }

  /** Set of related completion entries
    * @param tag set tag
    * @param completions set of unique completion entries
    */
  case class CompletionSet[M, TM](tag: CompletionTag[TM], completions: immutable.HashMap[Elems, Completion[M]]) {
    def label: String                          = tag.label
    def score: Int                             = tag.score
    def description: Option[String]            = tag.description
    def meta: Option[TM]                       = tag.meta
    def entries: Iterable[Completion[M]]       = completions.values
    def sortedEntries: Seq[Completion[M]]      = entries.toSeq.sorted
    def stringEntries: Seq[String]             = sortedEntries.map(_.value.toString)
    def map(f: Completion[M] => Completion[M]) = CompletionSet(tag, completions.values.map(f).toSeq)
    override def toString: String              = this.asJson.spaces2
    def toJson: Json                           = this.asJson
  }

  case object CompletionSet {
    def apply[M, TM](tag: CompletionTag[TM], completions: Seq[(Elems, Completion[M])]): CompletionSet[M, TM] =
      CompletionSet(tag, immutable.HashMap(completions: _*))

    def apply(tag: String, el: Elem): CompletionSet[Nothing, Nothing] =
      CompletionSet(CompletionTag(tag), Seq(Seq(el) -> Completion(el)))

    def apply(tag: String, elems: Elems): CompletionSet[Nothing, Nothing] =
      CompletionSet(CompletionTag(tag), Seq(elems -> Completion(elems)))

    def apply[M](tag: String, completion: Completion[M]): CompletionSet[M, Nothing] =
      CompletionSet(CompletionTag(tag), Seq(completion.value -> completion))

    def apply[M, TM](tag: CompletionTag[TM], completions: Iterable[Completion[M]]): CompletionSet[M, TM] =
      CompletionSet(tag, completions.map(c => c.value -> c).toSeq)

    def apply[M](tag: String, completions: Iterable[Completion[M]]): CompletionSet[M, Nothing] =
      CompletionSet(CompletionTag(tag), completions.map(c => c.value -> c).toSeq)

    def apply[M](completions: Iterable[Completion[M]]): CompletionSet[M, Nothing] =
      CompletionSet(CompletionTag.Default, completions.map(c => c.value -> c).toSeq)

    def apply(completions: Iterable[Completion[Nothing]]): CompletionSet[Nothing, Nothing] =
      CompletionSet(CompletionTag.Default, completions.map(c => c.value -> c).toSeq)

    def apply[M](completions: Completion[M]*): CompletionSet[M, Nothing] =
      CompletionSet(CompletionTag.Default, completions.map(c => c.value -> c))

    def apply[M](completions: Completion[Nothing]*): CompletionSet[Nothing, Nothing] =
      CompletionSet(CompletionTag.Default, completions.map(c => c.value -> c))

    def apply(el: Elem): CompletionSet[Nothing, Nothing] =
      CompletionSet(CompletionTag.Default, Seq(Seq(el) -> Completion(el)))

    def apply(completions: Traversable[Elems]): CompletionSet[Nothing, Nothing] =
      CompletionSet(CompletionTag.Default, completions.map(c => c -> Completion(c)).toSeq)

    implicit def orderingByScoreAndThenAlphabetical: Ordering[CompletionSet[_, _]] = Ordering.by(s => (-s.score, s.label))

    val empty = CompletionSet()
  }

  /** Completion entry
    * @param value entry value (e.g. string literal)
    * @param score entry score (defines the order of entries within a set, the higher the better)
    * @param meta entry meta (e.g. visual style)
    */
  case class Completion[M](value: Elems, score: Int = DefaultCompletionScore, meta: Option[M] = None) {
    require(value.nonEmpty, "empty completion")
    def withMeta[NM](newMeta: NM): Completion[NM] = copy(meta = Some(newMeta))

    def toJson: Json              = this.asJson
    override def toString: String = this.asJson.spaces2
  }
  case object Completion {
    def apply(el: Elem): Completion[Nothing] = Completion(Seq(el))
    implicit def orderingByScoreAndThenAlphabetical: Ordering[Completion[_]] =
      Ordering.by(c => (-c.score, c.value.toString))
  }

  /** Result of parser completion, listing the possible entry alternatives at a certain input position
    * @param position position in the input where completion entries apply
    * @param sets completion entries, grouped per tag
    */
  case class Completions[M, TM, GM](position: Position, meta: Option[GM], sets: immutable.HashMap[String, CompletionSet[M, TM]])(
      implicit mergeMeta: (M, M) => M,
      mergeTagMeta: (TM, TM) => TM,
      mergeGlobalMeta: (GM, GM) => GM) {
    def isEmpty: Boolean                                      = sets.isEmpty
    def nonEmpty: Boolean                                     = !isEmpty
    def setWithTag(tag: String): Option[CompletionSet[M, TM]] = sets.get(tag)
    def allSets: Iterable[CompletionSet[M, TM]]               = sets.values.toSeq.sorted
    def allCompletions: Iterable[Completion[_]]               = allSets.flatMap(_.sortedEntries)
    def defaultSet: Option[CompletionSet[M, TM]]              = sets.get("")
    def withMeta[NGM](newMeta: NGM): Completions[M, TM, NGM]  = copy(meta = Some(newMeta))
    def map[NM, NTM](f: CompletionSet[M, TM] => CompletionSet[NM, NTM])  = Completions(position, meta, sets.values.map(f).toSeq)

    override def toString: String = this.asJson.spaces2
    def toJson: Json              = this.asJson
    def setsToJson: Json          = allSets.asJson

    private def mergeMetaData[T](left: Option[T], right: Option[T])(merge: (T, T) => T) =
      if (left == right) left
      else
        (left, right) match {
          case (Some(l), Some(r)) => Some(merge(l, r))
          case (Some(l), None)    => Some(l)
          case (None, Some(r))    => Some(r)
          case (None, None)       => None
        }

    private def mergeCompletion(left: Completion[M], right: Completion[M]): Completion[M] = {
      if (left == right) {
        left
      } else {
        assert(left.value == right.value, "Attempt to merge different completion entries")
        Completion(
          left.value,
          left.score.max(right.score),
          mergeMetaData(left.meta, right.meta)(mergeMeta)
        )
      }
    }

    private def mergeSets(left: CompletionSet[M, TM], right: CompletionSet[M, TM]): CompletionSet[M, TM] = {
      if (left == right) {
        left
      } else {
        assert(left.label == right.label, "Attempt to merge sets with different completion tags")
        CompletionSet(
          CompletionTag(left.tag.label,
                        left.score.max(right.score),
                        left.description.orElse(right.description),
                        mergeMetaData(left.meta, right.meta)(mergeTagMeta)),
          left.completions.merged(right.completions)((l, r) => {
            val (leftLabel, leftCompletion) = l
            val (_, rightCompletion)        = r
            (leftLabel, mergeCompletion(leftCompletion, rightCompletion))
          })
        )
      }
    }

    def |(other: Completions[M, TM, GM]): Completions[M, TM, GM] = {
      other match {
        case Completions.empty => this
        case _ =>
          other.position match {
            case otherPos if otherPos < position => this
            case otherPos if otherPos == position =>
              Completions(
                position,
                mergeMetaData(meta, other.meta)(mergeGlobalMeta),
                sets.merged(other.sets)((l, r) => {
                  val (leftLabel, leftCompletion) = l
                  val (_, rightCompletions)       = r
                  (leftLabel, mergeSets(leftCompletion, rightCompletions))
                })
              )
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

    def takeTop(count: Int): Completions[M, TM, GM] = {
      val allEntries = allSets.toStream.flatMap(s => s.entries.toStream.map((_, s.tag)))
      val sortedEntries =
        lazyQuicksort(allEntries)(Ordering.by {
          case (Completion(_, score, _), CompletionTag(_, tagScore, _, _)) =>
            (-tagScore, -score)
        }).take(count).toList
      val regroupedSets = sortedEntries
        .groupBy { case (_, tag) => tag }
        .map {
          case (groupTag, completions) =>
            CompletionSet(groupTag, completions.map { case (c, _) => c })
        }
      Completions(position, meta, regroupedSets.map(s => s.tag.label -> s).toSeq)
    }

    def setsScoredWithMaxCompletion(): Completions[M, TM, GM] = {
      Completions(position, meta, sets.mapValues(s => CompletionSet(s.tag.copy(score = s.completions.values.map(_.score).max), s.completions)).toSeq)
    }
  }

  case object Completions {
    def apply[M, TM, GM](position: Position, meta: Option[GM], completionSets: Seq[(String, CompletionSet[M, TM])]): Completions[M, TM, GM] =
      Completions(position, meta, immutable.HashMap(completionSets: _*))

    def apply[M, TM](position: Position, completionSet: CompletionSet[M, TM]): Completions[M, TM, Nothing] =
      Completions(position, None, Seq(completionSet.tag.label -> completionSet))

    def apply[M, TM, GM](position: Position, meta: Option[GM], completionSet: CompletionSet[M, TM]): Completions[M, TM, GM] =
      Completions(position, meta, Seq(completionSet.tag.label -> completionSet))

    def apply[GM](position: Position, meta: Option[GM], completions: Traversable[Elems]): Completions[Nothing, Nothing, GM] =
      Completions(position, meta, CompletionSet(completions))

    def apply(position: Position, completions: Traversable[Elems]): Completions[Nothing, Nothing, Nothing] =
      Completions(position, None, CompletionSet(completions))

    def apply[M, TM, GM](position: Position, meta: Option[GM], completionSets: Iterable[CompletionSet[M, TM]]): Completions[M, TM, GM] =
      Completions(position, meta, completionSets.map(s => s.tag.label -> s).toSeq)

    def apply[M, TM](position: Position, completionSets: Iterable[CompletionSet[M, TM]]): Completions[M, TM, Nothing] =
      Completions(position, None, completionSets.map(s => s.tag.label -> s).toSeq)

    def apply[M, TM](completionSet: CompletionSet[M, TM]): Completions[M, TM, Nothing] =
      Completions(NoPosition, None, completionSet)

    def apply[M, TM](completionSets: Iterable[CompletionSet[M, TM]]): Completions[M, TM, Nothing] =
      Completions(NoPosition, None, completionSets.map(s => s.tag.label -> s).toSeq)

    def empty[M, TM] = Completions(NoPosition, None, immutable.HashMap[String, CompletionSet[M, TM]]())

  }

}
