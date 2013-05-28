package net.gadgil.finance.portfolio

import org.specs2.mutable._

class PortfolioToolsSpecification extends Specification {

  "portfolio parser" should {
    val theDefinition = """
      portfolio pf1 compare with "^GSPC" from 2013-04-10 to now
      long THRM USD 10000 at 11.14
      long LKQ USD 10000 at 23.20 stop-loss 50%
      long DORM USD 10000 at 38.86
      short CWEI USD 10000 at 12.91 stop-loss 100%
      short TEN USD 10000 at 41.19  stop-loss 100%
        """
    "parse test" in {
      val y = PortfolioProcessor.simulatePortfolio(theDefinition)
      println(y)
      val x = PortfolioDefinition.parseAll(PortfolioDefinition.portfolioStruct, theDefinition)
      println(x)
      x.get.name must be equalTo "pf1"
      x.get.positions(0).symbol must be equalTo "THRM"
      
    }
  }
}
