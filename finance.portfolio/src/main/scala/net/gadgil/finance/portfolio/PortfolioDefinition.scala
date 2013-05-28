package net.gadgil.finance.portfolio

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.matching.Regex
import org.joda.time.DateTime
import com.ning.http.client.AsyncHttpClient
import org.supercsv.io.CsvMapReader
import java.io.InputStreamReader
import org.supercsv.prefs.CsvPreference
import scala.collection.JavaConversions._
import scala.collection.immutable.SortedMap
import scala.collection.immutable.TreeMap
import scala.collection.SortedSet

class PortfolioDefinition {

}

object PortfolioDefinition extends JavaTokenParsers {
  def portfolioStruct: Parser[PortfolioInfo] = "portfolio" ~> ident ~ compareWith ~ dateRange ~ portfolioComposition ^^ {
    case portfolioName ~ theCompareWith ~ thePositionDuration ~ portfolioComposition =>
      PortfolioInfo(portfolioName, theCompareWith, thePositionDuration, portfolioComposition)
  }

  def compareWith: Parser[String] = "compare" ~> "with" ~> stringLiteral

  def dateRange: Parser[PositionDuration] = ("from" ~> flexDateDef) ~ ("to" ~> flexDateDef) ^^ {
    case fromDate ~ toDate => {
      //println("from: " + fromDate + " to: " + toDate)
      PositionDuration(fromDate, toDate)
    }
  }

  def portfolioComposition: Parser[Seq[PortfolioPosition]] = position.+
  def position: Parser[PortfolioPosition] = ("long" | "short") ~ ident ~ ident ~ decimalNumber ~ atPrice ~ stopLoss.? ^^ {
    case positionType ~ symbol ~ currencyCode ~ amount ~ thePrice ~ theStopLoss => {
      //println(positionType, symbol, amount, theDate)
      PortfolioPosition(positionType, symbol, amount.toDouble, stopLoss = theStopLoss)
    }
  }

  def atPrice: Parser[Double] = "at" ~> decimalNumber ^^ {
    case thePrice => {
      thePrice.toDouble
    }
  }

  def stopLoss: Parser[Double] = "stop-loss" ~> decimalNumber ~ "%".? ^^ {
    case theStopLoss ~ isPercent =>
      theStopLoss.toDouble * (if (isPercent.isDefined) { 0.01 } else { 1 })
  }
  
  def flexDateDef: Parser[DateTime] = dateDef | "now" ^^ {
    case x => new DateTime()
  }

  def dateDef: Parser[DateTime] = new Regex("([0-9]{4})-([0-9]{2})-([0-9]{2})") ^^ { theDate =>
    if (theDate == "now") {
      new DateTime()
    } else {
      val r = new Regex("([0-9]{4})-([0-9]{2})-([0-9]{2})")
      val y = List("DD")
      val theMatchedGroups = r.findFirstMatchIn(theDate).fold(y) { xxx => xxx.subgroups }
      new DateTime(theMatchedGroups(0).toInt, theMatchedGroups(1).toInt, theMatchedGroups(2).toInt, 0, 0)
    }
  }
}

//case class TradeDate(year: Int, month: Int, day: Int)
case class PortfolioInfo(name: String, compareWith: String, positionDuration: PositionDuration, positions: Seq[PortfolioPosition])
case class PortfolioPosition(positionType: String, symbol: String, amount: Double, stopLoss: Option[Double])
case class PositionDuration(fromDate: DateTime, toDate: DateTime)

object PortfolioProcessor {
  def getHistoricalPrices(symbol: String, fromDate: DateTime, toDate: DateTime = DateTime.now()) = {
    //val theCurrentDate = new DateTime()
    // Example: http://finance.yahoo.com/q/hp?s=MSFT&a=01&b=3&c=2005&d=01&e=3&f=2013&g=m
    // http://ichart.finance.yahoo.com/table.csv?s=MSFT&a=1&b=4&c=2006&d=1&e=4&f=2013&g=m&ignore=.csv
    // Example: http://finance.yahoo.com/q/hp?s=<SYMBOL>&a=<FROM-MONTH-1>&b=<FROM-DAY>&c=<FROM-YEAR>&d=<TO-MONTH-1>&e=<TO-DAY>&f=<TO-YEAR>&g=m
    val theUrlTemplate = "http://ichart.finance.yahoo.com/table.csv?s=%s&a=%d&b=%d&c=%d&d=%d&e=%d&f=%d&g=d&ignore=.csv"
    val theYahooHistoricalPriceUrl = theUrlTemplate.format(
      symbol,
      fromDate.monthOfYear().get() - 1,
      fromDate.dayOfMonth().get(),
      fromDate.year().get(),
      toDate.monthOfYear().get() - 1,
      toDate.dayOfMonth().get(),
      toDate.year().get())
    //println("the url: " + theYahooHistoricalPriceUrl + " for " + fromDate + " to " + toDate)
    val ahc = new AsyncHttpClient
    val theQueryParameters: Map[String, String] = Map("s" -> symbol,
      "a" -> String.valueOf(fromDate.monthOfYear().get() - 1),
      "b" -> String.valueOf(fromDate.dayOfMonth().get()),
      "c" -> String.valueOf(fromDate.year().get()),
      "d" -> String.valueOf(toDate.monthOfYear().get() - 1),
      "e" -> String.valueOf(toDate.dayOfMonth().get()),
      "f" -> String.valueOf(toDate.year().get()))
    val theGetRequest = ahc.prepareGet(theYahooHistoricalPriceUrl)
    theQueryParameters.map { f => theGetRequest.addQueryParameter(f._1, f._2) }
    val theCSV = theGetRequest.execute().get().getResponseBodyAsStream()
    val theCSVReader = new CsvMapReader(new InputStreamReader(theCSV), CsvPreference.STANDARD_PREFERENCE)
    //val theHeader = theCSVReader.getHeader(true)
    //final String[] header = mapReader.getHeader(true);
    //final CellProcessor[] processors = getProcessors();

    //val x:Seq[Map[String, String]] = theCSVReader.
    class CsvIterator(csvMapReader: CsvMapReader) extends Iterator[Map[String, String]] {
      val header = csvMapReader.getHeader(true)
      var _next: Map[String, String] = null;
      def next = {
        _next
      }

      def hasNext = {
        val x = csvMapReader.read(header: _*)
        if (x == null) {
          _next = null
          false
        } else {
          _next = x.toMap
          true
        }
      }
    }
    //    val iterator = Iterator.continually(theCSVReader.read(theHeader: _*)).takeWhile(_ != null)
    //    val x = iterator.map { m =>
    //      m.toMap
    //    }
    val theIterator = new CsvIterator(theCSVReader)
    // Return in chronological order
    val theChronologicalHistoricalPrices = theIterator.toSeq.reverse
    val theChronologicalTuples = theChronologicalHistoricalPrices.map { theRow =>
      (theRow("Date"), theRow)
    }
    SortedMap(theChronologicalTuples: _*)
  }

  def parsePortfolio(portfolioDefinitionString: String): PortfolioInfo = {
    val theParsedPortfolio = PortfolioDefinition.parseAll(PortfolioDefinition.portfolioStruct, portfolioDefinitionString)
    if (theParsedPortfolio.successful) {
      theParsedPortfolio.get
    } else {
      throw new IllegalArgumentException(theParsedPortfolio.toString())
    }
  }

  def simulatePortfolio(portfolioDefinitionString: String) = {
    val thePortfolioInfo = parsePortfolio(portfolioDefinitionString)
    // Usually an index symbol, but it does not have to be an index
    val theIndexSymbol = thePortfolioInfo.compareWith.substring(1, thePortfolioInfo.compareWith.length() - 1).replace("^", "%5E")
    val theHistoricalPricesForIndex = getHistoricalPrices(theIndexSymbol,
      thePortfolioInfo.positionDuration.fromDate,
      thePortfolioInfo.positionDuration.toDate)
    //val theActualStartDate = DateTime.parse(theHistoricalPricesForIndex(0)("Date"))

    // Map of symbols and historical positions  
    val theHistoricalPricesForPositions = (thePortfolioInfo.positions.map { thePosition =>
      (thePosition.symbol, getHistoricalPrices(thePosition.symbol,
        thePortfolioInfo.positionDuration.fromDate,
        thePortfolioInfo.positionDuration.toDate))
    }).toMap
    val theAvailableDates = theHistoricalPricesForIndex.keySet
    // find the result of subtracting all the keys from the index
    // (only dates that occur in all datasets)
    val theSetOfCommonDates = theHistoricalPricesForPositions.foldLeft(theAvailableDates) { (r, c) =>
      //println("subtracting: " + c._2.keySet + " from: " + r)
      r.intersect(c._2.keySet)
    }
    val theSequentialDates = theSetOfCommonDates.toSeq
    if (theSequentialDates.length == 0) {
      throw new IllegalArgumentException("No usable data found for portfolio definition: " + portfolioDefinitionString)
    }

    // Number of shares purchased
    // Negative number for short positions
    val numSharesMap = (thePortfolioInfo.positions.map { thePosition =>
      val theClosingPrice = theHistoricalPricesForPositions(thePosition.symbol)(theSequentialDates(0))("Close").toDouble
      val theShortAdjustment = if (thePosition.positionType == "short") { -1.0 } else { 1.0 }
      (thePosition.symbol, theShortAdjustment * thePosition.amount / theClosingPrice)
    }).toMap
    val xx = theSequentialDates.map { theDate =>
      calculateDailyPosition(theDate,
        numSharesMap,
        theHistoricalPricesForIndex,
        theHistoricalPricesForPositions,
        thePortfolioInfo.positions)
    }
    //println("found common dates: " + theSetOfCommonDates.slice(0, 10))
    //val symbolToHistoricalPricesMap = (theHistoricalPricesForPositions :+ (theIndexSymbol, theHistoricalPricesForIndex)).toMap
    // Now construct positions
    //thePortfolioInfo.positions(0).positionType
    xx
  }

  def calculateDailyPosition(date: String,
    numSharesMap: Map[String, Double],
    historicalPricesForIndex: SortedMap[String, Map[String, String]],
    theHistoricalPricesForPositions: Map[String, SortedMap[String, Map[String, String]]],
    positions: Seq[PortfolioPosition]) = {

    val theTotalAmountInvested = positions.foldLeft(0.0) { (theTotal, thePosition) =>
      theTotal + thePosition.amount
    }
    val theNumberOfIndexShares = theTotalAmountInvested/historicalPricesForIndex(historicalPricesForIndex.keySet.toSeq(0))("Close").toDouble
    //println("theTotalAmountInvested = " + theTotalAmountInvested)
    val theSymbolToAmountMap = (
      positions.map { p =>
        (p.symbol, p.amount)
      }).toMap
    val thePositionValue = numSharesMap.keySet.foldLeft(0.0) { (theTotal, theSymbol) =>
      val theNumShares = numSharesMap(theSymbol)
      if (theNumShares < 0) {
        // if this position is a short position, value should be subtracted from initial amount
        // since numShares will be negative, we add
        theTotal + (2 * theSymbolToAmountMap(theSymbol) + theHistoricalPricesForPositions(theSymbol)(date)("Close").toDouble * theNumShares)
      } else {
        theTotal + theHistoricalPricesForPositions(theSymbol)(date)("Close").toDouble * theNumShares
      }
    }
    DateToPositionMap(date, theNumberOfIndexShares * historicalPricesForIndex(date)("Close").toDouble, thePositionValue)
  }
}

case class DateToPositionMap(date: String, indexValue: Double, positionValue: Double)