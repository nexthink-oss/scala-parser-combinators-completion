[![Build status](https://travis-ci.org/nexthink/scala-parser-combinators-completion.svg?branch=master)](https://travis-ci.org/jchapuis/scala-parser-combinators-completion)
[![License](https://img.shields.io/:license-BSD3-blue.svg)](https://opensource.org/licenses/bsd-3-clause)
[![Codacy Badge](https://api.codacy.com/project/badge/Grade/387b9093be344220b72481cbf987cfae)](https://www.codacy.com/app/jchapuis/scala-parser-combinators-completion?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=jchapuis/scala-parser-combinators-completion&amp;utm_campaign=Badge_Grade)
[![Codacy Badge](https://api.codacy.com/project/badge/Coverage/387b9093be344220b72481cbf987cfae)](https://www.codacy.com/app/nexthink/scala-parser-combinators-completion?utm_source=github.com&utm_medium=referral&utm_content=nexthink/scala-parser-combinators-completion&utm_campaign=Badge_Coverage)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.nexthink/scala-parser-combinators-completion_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.nexthink/scala-parser-combinators-completion_2.12)

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
 libraryDependencies += "com.nexthink" %% "scala-parser-combinators-completion" % "1.0.9"
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
|`%?`    |defines the description of the completions tag|<code>("+" &#124; "-") %? "arithmetic operators"</code>|
|`%%`    |defines the completions tag meta-data (can be used to encode properties for the tag in JSON, e.g. visual decorations)|<code>("+" &#124; "-") %% ("style" -> "highlight")</code>|
|`%-%`   |defines the completion meta-data (can be used to encode properties for each completion entry in JSON, e.g. visual decorations)|<code>("+" %-% ("style" -> "highlight") &#124; "-")</code>|   

## Fuzzy completion

This library also provides special parsers which support fuzzy completion, present in the `TermsParsers` trait, by means of the `oneOfTermsFuzzy` method capable of fuzzing completion on the input to match a set of terms (note that parsing itself obviously requires an exact match and is fast thanks to a prefix trie lookup on each input char). For instance, with the following dummy grammar:

```scala
object Grammar extends TermsParsers {
  val fuzzyCountries = "my favourite country is " ~ oneOfTermsFuzzy(Seq("United States of America", "Afghanistan", "Albania", "Algeria", "Andorra", "Angola", "Antigua & Deps", "Argentina", "Armenia", "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain", "Bangladesh", "Barbados", "Belarus", "Belgium", "Belize", "Benin", "Bhutan", "Bolivia", "Bosnia Herzegovina", "Botswana", "Brazil", "Brunei", "Bulgaria", "Burkina", "Burma", "Burundi", "Cambodia", "Cameroon", "Canada", "Cape Verde", "Central African Rep", "Chad", "Chile", "People's Republic of China", "Republic of China", "Colombia", "Comoros", "Democratic Republic of the Congo", "Republic of the Congo", "Costa Rica,", "Croatia", "Cuba", "Cyprus", "Czech Republic", "Danzig", "Denmark", "Djibouti", "Dominica", "Dominican Republic", "East Timor", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Estonia", "Ethiopia", "Fiji", "Finland", "France", "Gabon", "Gaza Strip", "The Gambia", "Georgia", "Germany", "Ghana", "Greece", "Grenada", "Guatemala", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Holy Roman Empire", "Honduras", "Hungary", "Iceland", "India", "Indonesia", "Iran", "Iraq", "Republic of Ireland", "Israel", "Italy", "Ivory Coast", "Jamaica", "Japan", "Jonathanland", "Jordan", "Kazakhstan", "Kenya", "Kiribati", "North Korea", "South Korea", "Kosovo", "Kuwait", "Kyrgyzstan", "Laos", "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Liechtenstein", "Lithuania", "Luxembourg", "Macedonia", "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Malta", "Marshall Islands", "Mauritania", "Mauritius", "Mexico", "Micronesia", "Moldova", "Monaco", "Mongolia", "Montenegro", "Morocco", "Mount Athos", "Mozambique", "Namibia", "Nauru", "Nepal", "Newfoundland", "Netherlands", "New Zealand", "Nicaragua", "Niger", "Nigeria", "Norway", "Oman", "Ottoman Empire", "Pakistan", "Palau", "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Poland", "Portugal", "Prussia", "Qatar", "Romania", "Rome", "Russian Federation", "Rwanda", "St Kitts & Nevis", "St Lucia", "Saint Vincent & the", "Grenadines", "Samoa", "San Marino", "Sao Tome & Principe", "Saudi Arabia", "Senegal", "Serbia", "Seychelles", "Sierra Leone", "Singapore", "Slovakia", "Slovenia", "Solomon Islands", "Somalia", "South Africa", "Spain", "Sri Lanka", "Sudan", "Suriname", "Swaziland", "Sweden", "Switzerland", "Syria", "Tajikistan", "Tanzania", "Thailand", "Togo", "Tonga", "Trinidad & Tobago", "Tunisia", "Turkey", "Turkmenistan", "Tuvalu", "Uganda", "Ukraine", "United Arab Emirates", "United Kingdom", "Uruguay", "Uzbekistan", "Vanuatu", "Vatican City", "Venezuela", "Vietnam", "Yemen", "Zambia", "Zimbabwe"))
}
```

Performing the following completion:

```scala
Grammar.completeString(Grammar.fuzzyCountries, "my favourite country is Swtlz")
``` 

leads to this output:

```scala
List(Sweden, Swaziland, Switzerland)
```

`oneOfTerms` sets the similarity metric in the completion entry score, so that completions can be ordered:

```scala
Grammar.complete(Grammar.fuzzyCountries, "my favourite country is Thld")
``` 
 
leads to:

```json
{
  "position": {
    "line": 1,
    "column": 25
  },
  "sets": [
    {
      "tag": {
        "label": "",
        "score": 0
      },
      "completions": [
         {
          "value": "Thailand",
          "score": 43
        },
        {
          "value": "The Gambia",
          "score": 25
        },
        {
          "value": "Jonathanland",
          "score": 22
        },
        {
          "value": "Chad",
          "score": 20
        },
        {
          "value": "Togo",
          "score": 20
        }
      ]
    }
  ]
}
```

### `oneOfTermsFuzzy` parameters

Below the signature of the `oneOfTermsFuzzy` method:

```scala
 def oneOfTermsFuzzy(terms: Seq[String],
                     similarityMeasure: (String, String) => Double = diceSorensenSimilarity,
                     similarityThreshold: Int = DefaultSimilarityThreshold,
                     maxCompletionsCount: Int = DefaultMaxCompletionsCount)
```

 - `terms`: the list of terms to build the parser for
 - `similarityMeasure`: the string similarity metric to be used. Any `(String, String) => Double` function can be passed in, but the library provides DiceSorensen (default), JaroWinkler, Levenshtein & NgramDistance. Metric choice depends on factors such as type of terms, performance, etc. See below for more information about the underlying data structure.
 - `similarityThreshold`: the minimum similarity score for an entry to be considered as a completion candidate
 - `maxCompletionsCount`: maximum number of completions returned by the parser   

### Fuzzy matching technique
For fuzzy completion, terms are decomposed in their trigrams and stored in a map which indexes terms per trigram. This allows fast lookup of a set of completion candidates which share the same trigrams as the input. These candidates are ranked by the number of shared trigrams with the input, and a subset of the highest ranked candidates are kept. This selection of candidates is then re-evaluated with the specified similarity metric (`similarityMeasure`), which is assumed to be more precise (and thus slower).

The top candidates according to `maxCompletionsCount` are returned as completions. 

Note that terms are affixed so that the starting and ending two characters count more than the others, in order to favor completions which start or end with the same characters as the input.
  