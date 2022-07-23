package dev.insideyou
package moneycalc

import scala.util.chaining.*
import java.text.DecimalFormat

/** context/config:
  * 3 hardcoded currencies: BRL, EUR, USD
  * 6 hardcoded conversion rates (in v2 we could fetch/cache them)
  *
  * input:
  * first row will be a header with the currencies
  * 4 columns:
  *   1 - description
  *   2,3,4 (one currency each, ordered alphabetically, 1 value per currency)
  *
  * output:
  * input in each currency (10usd -> 10usd, 8eur, 50brl)
  * sum in all currencies
  *
  * v2: fetch/cache currencies
  * v2: calculate the sum as we move along
  * v2: sort by amount desc (use the first currency)
  */

enum Currency:
  case BRL, EUR, USD

type Amount = BigDecimal
type Rate = BigDecimal

def converted(
    from: Currency,
    to: Currency,
    amount: Amount,
  ): Amount =
  amount * rate(from, to)

def rate(from: Currency, to: Currency): Rate =
  from -> to match
    // case f -> t if f == t => 1
    case Currency.BRL -> Currency.BRL => 1
    case Currency.EUR -> Currency.BRL => 6
    case Currency.USD -> Currency.BRL => 5

    case Currency.BRL -> Currency.EUR => BigDecimal(1) / 6
    case Currency.EUR -> Currency.EUR => 1
    case Currency.USD -> Currency.EUR => BigDecimal("0.9")

    case Currency.BRL -> Currency.USD => BigDecimal(1) / 5
    case Currency.EUR -> Currency.USD => BigDecimal("1.1")
    case Currency.USD -> Currency.USD => 1

object Input:
  type Raw = List[String]
  type Parsed = List[ParsedRow]

  final case class ParsedRow(
      description: String,
      brl: Amount,
      eur: Amount,
      usd: Amount,
    )

object Output:
  type Raw = List[String]
  type Body = List[Row.Body]

  object Row:
    object Header:
      lazy val description: String = "description"
      lazy val brl: String = Currency.BRL.toString
      lazy val eur: String = Currency.EUR.toString
      lazy val usd: String = Currency.USD.toString

    final case class Body(
        description: String,
        brl: Amount,
        eur: Amount,
        usd: Amount,
      )

def parsedInput(raw: Input.Raw): Input.Parsed =
  raw.tail.map(parsedInputRow)

@scala.annotation.nowarn
def parsedInputRow(rawInputRow: String): Input.ParsedRow =
  rawInputRow.split(",", -1).nn.map(_.nn.trim.nn).toList match
    case l @ List(description, rawBrl, rawEur, rawUsd) =>
      Input.ParsedRow(description, parsed(rawBrl), parsed(rawEur), parsed(rawUsd))

def parsed(rawAmount: String): Amount =
  if rawAmount.isEmpty then 0 else BigDecimal(rawAmount)

def transformed(input: Input.Parsed): Output.Body =
  val body =
    input.map { inputRow =>
      Output
        .Row
        .Body(
          description = inputRow.description,
          brl = List(
            converted(Currency.BRL, Currency.BRL, inputRow.brl),
            converted(Currency.EUR, Currency.BRL, inputRow.eur),
            converted(Currency.USD, Currency.BRL, inputRow.usd),
          ).sum,
          eur = List(
            converted(Currency.BRL, Currency.EUR, inputRow.brl),
            converted(Currency.EUR, Currency.EUR, inputRow.eur),
            converted(Currency.USD, Currency.EUR, inputRow.usd),
          ).sum,
          usd = List(
            converted(Currency.BRL, Currency.USD, inputRow.brl),
            converted(Currency.EUR, Currency.USD, inputRow.eur),
            converted(Currency.USD, Currency.USD, inputRow.usd),
          ).sum,
        )
    }

  val total =
    val (brl, eur, usd) =
      body.foldLeft[(BigDecimal, BigDecimal, BigDecimal)]((0, 0, 0)) {
        case ((brl, eur, usd), current) =>
          (brl + current.brl, eur + current.eur, usd + current.usd)
      }

    Output.Row.Body(description = "Total", brl, eur, usd)

  body.appended(total)

lazy val format = new DecimalFormat("#,###.00");

def rendered(body: Output.Body): Output.Raw =
  def helper(in: Any): String =
    in.toString.padTo(15, " ").mkString

  val headerString =
    s"""${"Description".pipe(helper)} │ ${Currency.BRL.pipe(helper)} │ ${Currency.EUR.pipe(helper)} │ ${Currency.USD.pipe(helper)} """

  // TODO rename
  val bodyString =
    body.map { row =>
      s"${row.description.pipe(helper)} │ ${format
          .format(row.brl)
          .pipe(helper)} │ ${format.format(row.eur).pipe(helper)} │ ${format.format(row.usd).pipe(helper)}"
    }

  val bodyWithoutTotal = bodyString.dropRight(1)
  val total = bodyString.last
  val hyphens = "─" * 100

  headerString +: hyphens +: bodyWithoutTotal :+ hyphens :+ total

@main def Main(args: String*): Unit =
  println("─" * 100)

  val inputFromFile: Input.Raw =
    scala.io.Source.fromResource("input.csv").getLines.toList

  val input: Input.Raw =
    List(
      "Description,BRL,EUR,USD",
      "Spotify,,,10",
      "Netflix,,15,",
      "YouTube,20,,",
      "Apartment,123456.789,,",
      "Insurance,,0.12345,",
      "Car,,,0.456",
    )

  val result =
    inputFromFile
      .tap(_.foreach(println))
      .tap(_ => println())
      .pipe(parsedInput)
      .tap(_.foreach(println))
      .pipe(transformed)
      .tap(_ => println())
      .tap(_.foreach(println))
      .pipe(rendered)
      .tap(_ => println())
      .tap(_.foreach(println))
      .pipe(_.mkString("\n"))

  println()
  println(Console.GREEN + result + Console.RESET)

  println("─" * 100)
