[![Build status](https://api.travis-ci.org/jchapuis/scala-parser-combinators-completion.svg?branch=master)](https://travis-ci.org/jchapuis/scala-parser-combinators-completion)
[![License](https://img.shields.io/:license-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![scala-parser-combinators-completion](https://api.bintray.com/packages/jchapuis/maven/scala-parser-combinators-completion/images/download.svg) ](https://bintray.com/jchapuis/maven/scala-parser-combinators-completion/_latestVersion)

# scala-parser-combinators-completion
Completion support for scala parser combinators.

Mixing-in the `CompletionSupport` trait enables completion support for a grammar (use `RegexCompletionSupport` for `RegexParsers`):

```scala
object MyParsers extends RegexParsers with RegexCompletionSupport
```

Parsers are thus 'augmented' with a `completions` method which returns possible entry completions for a certain input. This can be used to elaborate as-you-type completions menus or tab-completion experiences, and is e.g. easy to plug with readline to implement a console application. 
A set of additional operators also allow overriding completions and specifying ordering and grouping properties for completions. 

## Adding an SBT dependency

Add the following lines to your `build.sbt` file:
 
 ```
 resolvers += Resolver.bintrayRepo("jchapuis/nexthink", "maven")
 libraryDependencies += "com.nexthink" %% "scala-parser-combinators-completion" % "1.0.0"
 ```

## Completing on a standard grammar
Below is a simple arithmetic expression grammar defined with parser combinators:

```scala
import com.nexthink.util.parsing.combinator.RegexParsers
object ExprParser extends RegexParsers with RegexCompletionSupport {
  val number = "[0-9]+".r
  lazy val expr = term ~
    rep(
      ("+" | "-") ~! term ^^ {
        case "+" ~ t => t
        case "-" ~ t => -t
      }) ^^ { case t ~ r => t + r.sum }
  lazy val term: Parser[Int] = factor ~
    rep(("*" | "/") ~! factor) ^^ {
    case f ~ Nil => f
    case f ~ r =>
      r.foldLeft(f) {
        case (prev, "*" ~ next) => prev * next
        case (prev, "/" ~ next) => prev / next
      }
  }
  lazy val factor = number ^^ { _.toInt } | "(" ~> expr <~ ")"
}
```
This grammar is able to parse and compute expressions such as 

```scala
ExprParser.parseAll(ExprParser.expr, "2+2")
ExprParser.parseAll(ExprParser.expr, "(10*2)/(5+5)")
```

leading to outputs

```
[1.4] parsed: 4
[1.13] parsed: 2
```

Additional methods `complete` and `completions` are now available in `RegexParsers` and `Parsers` respectively which list possible completions according to a certain input:
 
```
ExprParser.complete(ExprParser.expr, "2")
ExprParser.complete(ExprParser.expr, "2+")
ExprParser.complete(ExprParser.expr, "(10*2")
```

lead to outputs
```
Completions(1.2,Map( -> CompletionSet(,0,None,Set(Completion(*,0), Completion(/,0), Completion(+,0), Completion(-,0)))))
Completions(1.3,Map( -> CompletionSet(,0,None,Set(Completion((,0)))))
Completions(1.6,Map( -> CompletionSet(,0,None,Set(Completion(+,0), Completion(-,0), Completion(/,0), Completion(),0), Completion(*,0)))))
```

The first member of `Completions` is the position in the input at which the completions apply, e.g. `1.2` means second char on the first line. The second member is a map of `CompletionSet`, each set having an identifier, score and optional description and a set of `Completion` instances, each one scored as well. Note that in this case we have not specified any identifier for the completions thus they are mapped to the default set identifier (`""`), with default score (`0`). In such an example this type algebra might seem overkill, but in real completion-ready grammars this allows defining a rich completion experience for users. This will become cleared in the section below, where we'll add more context to the grammar to refine these completions. 
   
Such simple completions are easier to visualize with when calling `completionStrings` on those `Completions` instances:

```
res2: Seq[String] = List(*, +, -, /)
res3: Seq[String] = List(()
res4: Seq[String] = List(), *, +, -, /)
```

In other words
 * "`2`" completes to alternatives `*` `+` `-` `/`
 * "`2+`" completes to a single option  `(`
 * "`(10*2`" completes to alternatives `)` `*` `+` `-` `/`

## Decorated grammar

In the preceding example you might have noticed that completions were missing for numbers (since any number is supported by the parser) and there was no way of introducing distinctions between categories of completions (e.g. number, operators, delimiters, etc.) or defining an order in the returned completions.
This can be achieved with the help of a new set of operators which allow decorating the grammar with additional completion-related definitions. Below an decorated version of the same arithmetic expression grammar:

```scala
import com.nexthink.util.parsing.combinator.RegexParsers
object DecoratedExprParser extends RegexParsers with RegexCompletionSupport {
  val number = "[0-9]+".r %> ("1", "10", "99") % "number" %? "any number"
  lazy val expr = term ~
    rep(
      (("+" | "-") % "operators" %? "arithmetic operators" % 10) ~! term ^^ {
        case "+" ~ t => t
        case "-" ~ t => -t
      }) ^^ { case t ~ r => t + r.sum }
  lazy val term: Parser[Int] = factor ~
    rep((("*" | "/") % "operators" %? "arithmetic operators" % 10) ~! factor) ^^ {
    case f ~ Nil => f
    case f ~ r =>
      r.foldLeft(f) {
        case (prev, "*" ~ next) => prev * next
        case (prev, "/" ~ next) => prev / next
      }
  }
  lazy val factor = number ^^ { _.toInt } | ("(" % "delimiters") ~> expr <~ (")" % "delimiters")
}
```

Completing on the same expressions (`2`, `2+`, `(10*2`) now leads to the following results:

`2` completes to 
```Completions(1.2,
    Map(operators -> CompletionSet(operators,10,Some(arithmetic operators),
                        Set(Completion(*,0), Completion(/,0), Completion(+,0), Completion(-,0)))))
```

`2+` completes to 
```Completions(1.3,
    Map(number -> CompletionSet(number,0,Some(any number),
                    Set(Completion(1,0), Completion(10,0), Completion(99,0))), 
        delimiters -> CompletionSet(delimiters,0,None,
                    Set(Completion((,0)))))
```

`(10*2` completes to 
```Completions(1.6,
    Map(operators -> CompletionSet(operators,10,Some(arithmetic operators), 
                        Set(Completion(*,0), Completion(/,0), Completion(+,0), Completion(-,0))), 
        delimiters -> CompletionSet(delimiters,0,None,
                        Set(Completion(),0)))))
```

## Completion operators

|Operator|Description|Example|
|--------|-----------|-------|
|`%>`    |defines an explicit set of possible completions, e.g. to give examples|`"[0-9]+".r %> ("1", "10", "99")`|
|`%`     |defines the category of completions (i.e. completion set name)|<code>("+" &#124; "-") % "operators"</code>|
|`%`     |followed by an `Int`, defines the completion set score|<code>("+" &#124; "-") % 10</code>|
|`%?`    |provides a description to the completion set|<code>("+" &#124; "-") %? "arithmetic operators"</code>|   

## Completion types
```scala
case class Completion(value: Elems, score: Int)
case class CompletionSet(tag: String,
                         score: Int,
                         description: Option[String],
                         completions: Set[Completion])
case class Completions(position: Position, sets: Map[String, CompletionSet])
```
