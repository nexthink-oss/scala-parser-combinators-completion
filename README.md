[![Build status](https://travis-ci.org/jchapuis/scala-parser-combinators-completion.svg?branch=master)](https://travis-ci.org/jchapuis/scala-parser-combinators-completion)
[![License](https://img.shields.io/:license-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![Download](https://api.bintray.com/packages/jchapuis/maven/scala-parser-combinators-completion/images/download.svg) ](https://bintray.com/jchapuis/maven/scala-parser-combinators-completion/_latestVersion)

# scala-parser-combinators-completion
Completion support for scala parser combinators.

Mixing-in the `CompletionSupport` trait enables completion support for a grammar (use `RegexCompletionSupport` for `RegexParsers`):

```scala
object MyParsers extends RegexParsers with RegexCompletionSupport
```

Parsers are thus 'augmented' with a `completions` method which returns possible entry completions for a certain input. This can be used to elaborate as-you-type completions menus or tab-completion experiences, and is e.g. easy to plug with readline to implement a console application. 
A set of additional operators also allow overriding completions and specifying ordering and grouping properties for completions. 

## Adding a SBT dependency

Add the following lines to your `build.sbt` file:
 
 ```
 resolvers += Resolver.bintrayRepo("jchapuis", "maven")
 libraryDependencies += "com.nexthink" %% "scala-parser-combinators-completion" % "1.0.0"
 ```

## Completing on a grammar
Below is a simple arithmetic expression grammar defined with parser combinators:

```scala
import com.nexthink.utils.parsing.combinator.RegexParsers
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

Methods `complete` and `completeString` are available in `RegexCompletionSupport` which list possible completions according to a certain input:
 
```
ExprParser.completeString(ExprParser.expr, "2")
ExprParser.completeString(ExprParser.expr, "2+")
ExprParser.completeString(ExprParser.expr, "(10*2")
```

lead to outputs

```
res2: Seq[String] = List(*, +, -, /)
res3: Seq[String] = List(()
res4: Seq[String] = List(), *, +, -, /)
```

In other words
 * "`2`" completes to alternatives `*` `+` `-` `/`
 * "`2+`" completes to a single option  `(`
 * "`(10*2`" completes to alternatives `)` `*` `+` `-` `/`

(note that using the method `complete` with the same inputs would lead to structured completion results rather than plain strings, see below for more details)

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
```json
{
     "position":{
       "line":1,
       "column":2
     },
     "sets":[{
       "tag":{
         "label":"operators",
         "score":10,
         "description":"arithmetic operators"
       },
       "completions":[{
         "value":"*",
         "score":10
       },{
         "value":"/",
         "score":10
       },{
         "value":"+",
         "score":10
       },{
         "value":"-",
         "score":10
       }]
     }]
   }
```

`2+` completes to 
```json
{
  "position":{
    "line":1,
    "column":3
  },
  "sets":[{
    "tag":{
      "label":"number",
      "score":0,
      "description":"any number"
    },
    "completions":[{
      "value":"1",
      "score":0
    },{
      "value":"10",
      "score":0
    },{
      "value":"99",
      "score":0
    }]
  },{
    "tag":{
      "label":"",
      "score":0
    },
    "completions":[{
      "value":"(",
      "score":0
    }]
  }]
}
```

`(10*2` completes to 
```json
{
  "position":{
    "line":1,
    "column":6
  },
  "sets":[{
    "tag":{
      "label":"operators",
      "score":10,
      "description":"arithmetic operators"
    },
    "completions":[{
      "value":"*",
      "score":10
    },{
      "value":"/",
      "score":10
    },{
      "value":"+",
      "score":10
    },{
      "value":"-",
      "score":10
    }]
  },{
    "tag":{
      "label":"",
      "score":0
    },
    "completions":[{
      "value":")",
      "score":0
    }]
  }]
}
```

## Completion operators

|Operator|Description|Example|
|--------|-----------|-------|
|`%>`    |defines an explicit set of possible completions, e.g. to give examples|`"[0-9]+".r %> ("1", "10", "99")`|
|`%`     |defines the completions tag|<code>("+" &#124; "-") % "operators"</code>|
|`%`     |followed by an `Int`, defines the completion tag score|<code>("+" &#124; "-") % 10</code>|
|`%?`    |provides a description to the completion set|<code>("+" &#124; "-") %? "arithmetic operators"</code>|
|`%%`    |defines the completion tag kind (can be used to encode properties for the tag, e.g. visual decorations)|<code>("+" &#124; "-") %% "style: highlight"</code>|
|`%-%`   |defines the completion kind (can be used to encode properties for each completion entry, e.g. visual decorations)|<code>("+" %-% "style: highlight" &#124; "-")</code>|   
