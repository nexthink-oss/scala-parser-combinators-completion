package com.nexthink.utils.parsing.combinator.completion

import com.nexthink.utils.meta.MetaSemigroup
import com.nexthink.utils.meta.MetaImplicits

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
trait CompletionTypes extends MetaImplicits {
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
    def withMeta[NM](newMeta: NM): CompletionTag[NM] = copy(meta = Some(newMeta))
  }

  case object CompletionTag {
    def default[M] =
      CompletionTag[M](DefaultCompletionTag, DefaultCompletionScore, None, None)
    def apply[M](label: String): CompletionTag[M] =
      CompletionTag(label, DefaultCompletionScore, None, None)
    def apply[M](label: String, meta: M): CompletionTag[M] =
      CompletionTag(label, DefaultCompletionScore, None, Some(meta))
    def apply[M](label: String, score: Int): CompletionTag[M] =
      CompletionTag(label, score, None, None)
    def apply[M](label: String, score: Int, meta: M): CompletionTag[M] =
      CompletionTag(label, score, None, Some(meta))
    def apply[M](label: String, score: Int, description: String): CompletionTag[M] =
      CompletionTag(label, score, Some(description), None)
  }

  /** Set of related completion entries
    * @param tag set tag
    * @param completions set of unique completion entries
    */
  case class CompletionSet[M](tag: CompletionTag[M], completions: immutable.HashMap[Elems, Completion[M]]) {
    def label: String                          = tag.label
    def score: Int                             = tag.score
    def description: Option[String]            = tag.description
    def meta: Option[M]                        = tag.meta
    def entries: Iterable[Completion[M]]       = completions.values
    def sortedEntries: Seq[Completion[M]]      = entries.toSeq.sorted
    def stringEntries: Seq[String]             = sortedEntries.map(_.value.toString)
    def map(f: Completion[M] => Completion[M]) = CompletionSet(tag, completions.values.map(f).toSeq)
  }

  case object CompletionSet {
    def apply[M](tag: CompletionTag[M], completions: Seq[(Elems, Completion[M])]): CompletionSet[M] =
      CompletionSet(tag, immutable.HashMap(completions: _*))

    def apply[M](tag: String, el: Elem): CompletionSet[M] =
      CompletionSet(CompletionTag[M](tag), Seq(Seq(el) -> Completion[M](el)))

    def apply[M](tag: String, elems: Elems): CompletionSet[M] =
      CompletionSet(CompletionTag[M](tag), Seq(elems -> Completion[M](elems)))

    def apply[M](tag: String, completion: Completion[M]): CompletionSet[M] =
      CompletionSet(CompletionTag[M](tag), Seq(completion.value -> completion))

    def apply[M](tag: CompletionTag[M], completions: Iterable[Completion[M]]): CompletionSet[M] =
      CompletionSet(tag, completions.map(c => c.value -> c).toSeq)

    def apply[M](tag: String, completions: Iterable[Completion[M]]): CompletionSet[M] =
      CompletionSet(CompletionTag[M](tag), completions.map(c => c.value -> c).toSeq)

    def apply[M](completions: Iterable[Completion[M]]): CompletionSet[M] =
      CompletionSet(CompletionTag.default[M], completions.map(c => c.value -> c).toSeq)

    def apply[M](completions: Completion[M]*): CompletionSet[M] =
      CompletionSet(CompletionTag.default[M], completions.map(c => c.value -> c))

    def apply[M](el: Elem): CompletionSet[M] =
      CompletionSet(CompletionTag.default[M], Seq(Seq(el) -> Completion[M](el)))

    def apply[M](completions: Traversable[Elems]): CompletionSet[M] =
      CompletionSet(CompletionTag.default[M], completions.map(c => c -> Completion[M](c)).toSeq)

    implicit def orderingByScoreAndThenAlphabetical[M]: Ordering[CompletionSet[M]] = Ordering.by(s => (-s.score, s.label))

    def empty[M] = new CompletionSet[M](CompletionTag.default[M], immutable.HashMap())
  }

  /** Completion entry
    * @param value entry value (e.g. string literal)
    * @param score entry score (defines the order of entries within a set, the higher the better)
    * @param meta entry meta (e.g. visual style)
    */
  case class Completion[M](value: Elems, score: Int = DefaultCompletionScore, meta: Option[M] = None) {
    require(value.nonEmpty, "empty completion")
    def withMeta[NM](newMeta: NM): Completion[NM] = copy(meta = Some(newMeta))
  }
  case object Completion {
    def apply[M](el: Elem): Completion[M]   = Completion[M](Seq(el))
    def apply(els: Elems): Completion[Unit] = apply[Unit](els)
    implicit def orderingByScoreAndThenAlphabetical[M]: Ordering[Completion[M]] =
      Ordering.by(c => (-c.score, c.value.toString))
  }

  /** Result of parser completion, listing the possible entry alternatives at a certain input position
    * @param position position in the input where completion entries apply
    * @param sets completion entries, grouped per tag
    */
  case class Completions[M](position: Position, meta: Option[M], sets: immutable.HashMap[String, CompletionSet[M]])(implicit semigroup: MetaSemigroup[M]) {
    def isEmpty: Boolean                                  = sets.isEmpty
    def nonEmpty: Boolean                                 = !isEmpty
    def setWithTag(tag: String): Option[CompletionSet[M]] = sets.get(tag)
    def allSets: Iterable[CompletionSet[M]]               = sets.values.toSeq.sorted
    def allCompletions: Iterable[Completion[_]]           = allSets.flatMap(_.sortedEntries)
    def defaultSet: Option[CompletionSet[M]]              = sets.get("")
    def withMeta(newMeta: M): Completions[M]              = copy(meta = Some(newMeta))
    def map(f: CompletionSet[M] => CompletionSet[M])      = Completions(position, meta, sets.values.map(f).toSeq)

    private def mergeMetaData(left: Option[M], right: Option[M]) =
      if (left == right) left
      else
        (left, right) match {
          case (Some(l), Some(r)) => Some(semigroup.combine(l, r))
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
          mergeMetaData(left.meta, right.meta)
        )
      }
    }

    private def mergeSets(left: CompletionSet[M], right: CompletionSet[M]): CompletionSet[M] = {
      if (left == right) {
        left
      } else {
        assert(left.label == right.label, "Attempt to merge sets with different completion tags")
        CompletionSet(
          CompletionTag(left.tag.label, left.score.max(right.score), left.description.orElse(right.description), mergeMetaData(left.meta, right.meta)),
          left.completions.merged(right.completions)((l, r) => {
            val (leftLabel, leftCompletion) = l
            val (_, rightCompletion)        = r
            (leftLabel, mergeCompletion(leftCompletion, rightCompletion))
          })
        )
      }
    }

    def |(other: Completions[M]): Completions[M] = {
      if (other == Completions.empty[M])
        this
      else
        other.position match {
          case otherPos if otherPos < position => this
          case otherPos if otherPos == position =>
            Completions(
              position,
              mergeMetaData(meta, other.meta),
              sets.merged(other.sets)((l, r) => {
                val (leftLabel, leftCompletion) = l
                val (_, rightCompletions)       = r
                (leftLabel, mergeSets(leftCompletion, rightCompletions))
              })
            )
          case _ => other
        }
    }

    def completionStrings: Seq[String] =
      sets.values.toStream
        .sortBy(_.score)
        .reverse
        .flatMap(_.stringEntries)
        .toList

    def takeTop(count: Int): Completions[M] = {
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

    def setsScoredWithMaxCompletion(): Completions[M] = {
      Completions(position, meta, sets.mapValues(s => CompletionSet(s.tag.copy(score = s.completions.values.map(_.score).max), s.completions)).toSeq)
    }
  }

  case object Completions {
    def apply[M](position: Position, meta: Option[M], completionSets: Seq[(String, CompletionSet[M])])(implicit semigroup: MetaSemigroup[M]): Completions[M] =
      Completions(position, meta, immutable.HashMap(completionSets: _*))

    def apply[M](position: Position, completionSet: CompletionSet[M])(implicit semigroup: MetaSemigroup[M]): Completions[M] =
      Completions(position, None, Seq(completionSet.tag.label -> completionSet))

    def apply[M](position: Position, meta: Option[M], completionSet: CompletionSet[M])(implicit semigroup: MetaSemigroup[M]): Completions[M] =
      Completions(position, meta, Seq(completionSet.tag.label -> completionSet))

    def apply[M](position: Position, meta: Option[M], completions: Traversable[Elems])(implicit semigroup: MetaSemigroup[M]): Completions[M] =
      Completions(position, meta, CompletionSet[M](completions))

    def apply[M](position: Position, completions: Traversable[Elems])(implicit semigroup: MetaSemigroup[M]): Completions[M] =
      Completions(position, None, CompletionSet[M](completions))

    def apply[M](position: Position, meta: Option[M], completionSets: Iterable[CompletionSet[M]])(implicit semigroup: MetaSemigroup[M]): Completions[M] =
      Completions(position, meta, completionSets.map(s => s.tag.label -> s).toSeq)

    def apply[M](position: Position, completionSets: Iterable[CompletionSet[M]])(implicit semigroup: MetaSemigroup[M]): Completions[M] =
      Completions(position, None, completionSets.map(s => s.tag.label -> s).toSeq)

    def apply[M](completionSet: CompletionSet[M])(implicit semigroup: MetaSemigroup[M]): Completions[M] =
      Completions(NoPosition, None, completionSet)

    def apply[M](completionSets: Iterable[CompletionSet[M]])(implicit semigroup: MetaSemigroup[M]): Completions[M] =
      Completions(NoPosition, None, completionSets.map(s => s.tag.label -> s).toSeq)

    def empty[M](implicit semigroup: MetaSemigroup[M]): Completions[M] =
      Completions(NoPosition, None, immutable.HashMap[String, CompletionSet[M]]())
  }

}
