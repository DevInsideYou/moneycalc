package dev.insideyou
package moneycalc

import upickle.default.*

trait CurrencyFetcher:
  def getRates: ((String, String)) => Rate

object CurrencyFetcher:
  def make(apiToken: String): CurrencyFetcher =
    new:
      lazy val getRates: ((String, String)) => Rate =
        val response = Currency.values.map(makeApiCall).toList

        response.foldLeft(Map.empty) {
          case (acc, currentResponse) =>
            currentResponse.rates.foldLeft(acc) {
              case (acc2, to -> rate) =>
                acc2.updated(currentResponse.base -> to, rate)
            }
        }

      private def makeApiCall(base: Currency): Response =
        val response =
          requests
            .get(
              "https://api.apilayer.com/fixer/latest",
              headers = Map("apikey" -> apiToken),
              params = Map("base" -> base.toString, "symbols" -> Currency.values.mkString(", ")),
            )

        // val response =
        //   """|{
        //      |  "success": true,
        //      |  "timestamp": 1659804483,
        //      |  "base": "USD",
        //      |  "date": "2022-08-06",
        //      |  "rates": {
        //      |    "BRL": 5.164263,
        //      |    "EUR": 0.982295,
        //      |    "USD": 1
        //      |  }
        //      |}""".stripMargin

        val parsedRawResponse =
          read[Raw.Response](response)

        Response(
          timestamp = parsedRawResponse.timestamp,
          base = parsedRawResponse.base,
          rates = Map(
            "BRL" -> parsedRawResponse.rates.BRL,
            "EUR" -> parsedRawResponse.rates.EUR,
            "USD" -> parsedRawResponse.rates.USD,
          ),
        )

object Raw:
  final case class Response(
      timestamp: Long,
      base: String,
      rates: Rates,
    )

  object Response:
    given Reader[Response] =
      macroR

  final case class Rates(
      BRL: BigDecimal,
      EUR: BigDecimal,
      USD: BigDecimal,
    )

  object Rates:
    given Reader[Rates] =
      given Reader[BigDecimal] =
        reader[ujson.Value].map[BigDecimal](json => BigDecimal(json.value.toString))

      macroR

final case class Response(
    timestamp: Long,
    base: String,
    rates: Map[String, BigDecimal],
  )
