package dev.insideyou
package moneycalc

import scala.util.chaining.*
import java.text.*
import java.nio.charset.*

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
  * v2: ✅ fetch currencies
  * v2: ✅ clean up some stuff a little
  * v2: ✅ pretify the output
  * v2: ✅ configure scalafmt to not indent :+, +:
  * v2: ✅ read input from main.args instead of resources
  * v2: ✅ output to a file in a local directory
  *
  * v3: cache currencies
  * v3: calculate the sum as we move along
  * v3: sort by amount desc (use the first currency) (or at least ensure that it's already sorted
  */

enum Currency:
  case BRL, EUR, USD

type Amount = BigDecimal
type Rate = BigDecimal

def converted(
    rates: Rates
  )(
    from: Currency,
    to: Currency,
    amount: Amount,
  ): Amount =
  amount * rates.rate(from, to)

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
  rawInputRow.split(",", -1).map(_.trim) match
    case Array(description, rawBrl, rawEur, rawUsd) =>
      Input.ParsedRow(description, rawBrl.parsed, rawEur.parsed, rawUsd.parsed)

extension (rawAmount: String)
  private def parsed: Amount =
    if rawAmount.isEmpty then 0 else BigDecimal(rawAmount)

def transformed(rates: Rates)(input: Input.Parsed): Output.Body =
  val body =
    input.map { inputRow =>
      require(
        requirement = List(inputRow.brl, inputRow.eur, inputRow.usd).count(_ != 0) == 1,
        message = s"${inputRow.description} input had multiple values. {inputRow = $inputRow}.",
      )

      Output
        .Row
        .Body(
          description = inputRow.description,
          brl = List(
            converted(rates)(Currency.BRL, Currency.BRL, inputRow.brl),
            converted(rates)(Currency.EUR, Currency.BRL, inputRow.eur),
            converted(rates)(Currency.USD, Currency.BRL, inputRow.usd),
          ).sum,
          eur = List(
            converted(rates)(Currency.BRL, Currency.EUR, inputRow.brl),
            converted(rates)(Currency.EUR, Currency.EUR, inputRow.eur),
            converted(rates)(Currency.USD, Currency.EUR, inputRow.usd),
          ).sum,
          usd = List(
            converted(rates)(Currency.BRL, Currency.USD, inputRow.brl),
            converted(rates)(Currency.EUR, Currency.USD, inputRow.eur),
            converted(rates)(Currency.USD, Currency.USD, inputRow.usd),
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

def rendered(f: Format)(body: Output.Body): Output.Raw =
  def h(in: Any): String =
    in.toString.padTo(15, " ").mkString

  val headerString =
    s"""${h("Description")} │ ${h(Currency.BRL)} │ ${h(Currency.EUR)} │ ${h(Currency.USD)}"""

  val bodyString =
    body.map { row =>
      s"${h(row.description)} │ ${h(f.format(row.brl))} │ ${h(f.format(row.eur))} │ ${h(f.format(row.usd))}"
    }

  val bodyWithoutTotal = bodyString.dropRight(1)
  val total = bodyString.last

  extension (self: String)
    infix def withCrossAt(index: Int): String =
      self.updated(index, '┼')

    infix def withTsAt(index: Int): String =
      self.updated(index, '┬')

    infix def withInvertedTsAt(index: Int): String =
      self.updated(index, '┴')

  def horizontalLine(f: (String, Int) => String): String =
    List(16, 34, 52).foldLeft("─" * 70)(f)

  val hyphensWithCrosses =
    horizontalLine(Function.uncurried(withCrossAt))

  val hyphensWithTs =
    horizontalLine(Function.uncurried(withTsAt))

  val hyphensWithInvertedTs =
    horizontalLine(Function.uncurried(withInvertedTsAt))

  hyphensWithTs +:
  headerString +:
  hyphensWithCrosses +:
  bodyWithoutTotal :+
  hyphensWithCrosses :+
  total :+
  hyphensWithInvertedTs
end rendered

def writeToStdOut(string: String): Unit =
  println(Console.GREEN + string + Console.RESET)

def renderedAsCsv(f: Format)(body: Output.Body): Output.Raw =
  val headerString =
    s""""Description","${Currency.BRL}","${Currency.EUR}","${Currency.USD}""""

  val bodyString =
    body.map { row =>
      s""""${row.description}","${f.format(row.brl)}","${f.format(row.eur)}","${f.format(row.usd)}""""
    }

  val bodyWithoutTotal = bodyString.dropRight(1)
  val total = bodyString.last

  headerString :: total :: bodyWithoutTotal

def writeToFile(string: String): Unit =
  import java.nio.file.*
  import java.nio.charset.StandardCharsets

  Files.write(Paths.get("output.csv"), string.getBytes(StandardCharsets.UTF_8))

@main def Main(args: String*): Unit =
  val inputFilePath: Option[String] =
    args.headOption

  val inputFromFile: Input.Raw =
    inputFilePath
      .map(scala.io.Source.fromFile)
      .getOrElse(scala.io.Source.fromResource("input.csv"))
      .getLines
      .toList

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

  val rates: Rates =
    val client =
      CurrencyFetcher.make(
        apiToken = System.getenv("API_TOKEN")
      )

    Rates.make(client)

  // val rates: Rates =
  //   new Rates {
  //     @scala.annotation.nowarn
  //     def rate(from: Currency, to: Currency): Rate =
  //       from -> to match
  //         case f -> t if f == t => 1

  //         case Currency.EUR -> Currency.BRL => 6
  //         case Currency.USD -> Currency.BRL => 5

  //         case Currency.BRL -> Currency.EUR => BigDecimal(1) / 6
  //         case Currency.USD -> Currency.EUR => BigDecimal("0.9")

  //         case Currency.BRL -> Currency.USD => BigDecimal(1) / 5
  //         case Currency.EUR -> Currency.USD => BigDecimal("1.1")
  //   }

  val format: Format =
    new DecimalFormat("#,###.00");

  inputFromFile
    .pipe(parsedInput)
    .pipe(transformed(rates))
    .tap(
      _.pipe(rendered(format))
        .pipe(_.mkString("\n"))
        .tap(writeToStdOut)
    )
    .tap(
      _.pipe(renderedAsCsv(format))
        .pipe(_.mkString(start = "", sep = "\n", end = "\n"))
        .tap(writeToFile)
    )
end Main
