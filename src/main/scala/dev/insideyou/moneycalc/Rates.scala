package dev.insideyou
package moneycalc

trait Rates:
  def rate(from: Currency, to: Currency): Rate

object Rates:
  def make(client: CurrencyFetcher): Rates =
    new:
      val rates = client.getRates

      override def rate(from: Currency, to: Currency): Rate =
        rates(from.toString -> to.toString)
