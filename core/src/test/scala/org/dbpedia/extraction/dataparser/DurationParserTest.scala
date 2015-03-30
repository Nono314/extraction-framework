package org.dbpedia.extraction.dataparser

import org.scalatest.FlatSpec
import org.dbpedia.extraction.wikiparser._
import org.dbpedia.extraction.sources.WikiPage
import org.dbpedia.extraction.ontology.OntologyDatatypes
import org.dbpedia.extraction.util.Language
import org.scalatest.Matchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.BeMatcher
import org.scalatest.matchers.MatchResult
import scala.language.reflectiveCalls

@RunWith(classOf[JUnitRunner])
class DurationParserTest extends FlatSpec with Matchers
{
    "DurationParser" should "return 5 seconds for '5 secs'" in
     {
         parse("en", "Time", "5 secs").get should equal (5.0)
     }
    it should "return 10 minutes for 'context 10 min context'" in
     {
         parse("en", "Time", "context 10 min context").get should equal (600.0)
     }
    it should "return 14820 seconds for 'context 4 hours, 7 minutes context'" in
     {
         parse("en", "Time", "context 4 hours, 7 minutes context").get should equal (14820.0)
     }
    it should "return 14730 seconds for ' 4h 5m, 30s '" in
     {
         parse("en", "Time", " 4h 5m, 30s ").get should equal (14730.0)
     }
    it should "return 857752817 seconds for 'ganze 27 years, 2 months, 5 days, 1h 40min., 17s hat es gedauert '" in
     {
         parse("en", "Time", "ganze 27 years, 2 months, 5 days, 1h 40min., 17s hat es gedauert ").get should be (approximatelyEqualTo(857752817.0))
     }
    it should "return 188 seconds for 'context 3m 8s context'" in
     {
         parse("en", "Time", "context 3m 8s context").get should equal (188.0)
     }
    it should "return 2417 seconds for '40min., 17s'" in
     {
         parse("en", "Time", "40min., 17s").get should equal (2417.0)
     }
    it should "return aprox. 32263698 seconds for '1y 8d 4h 7m 8s'" in
     {
         parse("en", "Time", "1y 8d 4h 7m 8s").get should be (approximatelyEqualTo(32263628.0))
     }
    it should "return 604997.0 seconds for '27 years, 2 GLUCK, 5 days, 1hrs. 40min., 17s'" in
     {
         parse("en", "Time", "7 days, 5 DOLLARS, 3 min., 8 METERS, 17s").get should equal (604997.0)
     }
    // don't match characters after units
    it should "return 660 seconds for '11 min '" in
     {
         parse("en", "Time", "11 min ").get should equal (660.0)
     }
    it should "return 660 seconds for '(11 min) asd'" in
     {
         parse("en", "Time", "(11 min)").get should equal (660.0)
     }
    it should "return 660 seconds for '11 min,'" in
     {
         parse("en", "Time", "11 min,").get should equal (660.0)
     }
    it should "return 660 seconds for '11 min/'" in
     {
         parse("en", "Time", "11 min/").get should equal (660.0)
     }
    it should "return 86400 seconds for '1d/'" in
     {
         parse("en", "Time", "1d/").get should equal (86400.0)
     }
    // Dots, commas, spaces
    it should "return 435649 seconds for '5.042 234 days'" in
     {
         parse("en", "Time", "5.042 234 days").get should equal (435649.01759999996)
     }
    it should "return 48124803628 seconds for '577,000.042 d'" in
     {
         parse("en", "Time", "577,000.042 d").get should equal (49852803628.8)
     }
    it should "return 48124803628 seconds for '577.000,042 Tage'" in
     {
         parse("de", "Time", "577.000,042 Tage").get should equal (49852803628.8)
     }
    it should "return 9954414708530 seconds for '115,213,133.2 days, 50 secs'" in
     {
         parse("en", "Time", "115,213,133.2 days, 50 secs").get should equal (9954414708530.0)
     }
    // Implicit unit format 
    it should "return 273 seconds for '4 min. 33'" in
     {
         parse("fr", "Time", "4 min. 33").get should equal (273.0)
     }
    it should "return 5400 seconds for '1h30'" in
     {
         parse("fr", "Time", "1h30").get should equal (5400.0)
     }
    it should "return 4010 seconds for '66'50'" in
     {
         parse("fr", "Time", "66'50").get should equal (4010.0)
     }
    // Colon separated
    it should "return 30 seconds for ':30'" in
     {
         parse("en", "Time", ":30").get should equal (30.0)
     }
    it should "return 30 seconds for ':30 sec'" in
     {
         parse("en", "Time", ":30 sec").get should equal (30.0)
     }
    it should "return 90 seconds for '1:30'" in
     {
         parse("en", "Time", "1:30 min").get should equal (90.0)
     }
    it should "return 90 minutes for '1:30 h'" in
     {
         parse("en", "Time", "1:30 h").get should equal (5400.0)
     }
    it should "return 15030 seconds for '4:10:30'" in
     {
         parse("en", "Time", "4:10:30").get should equal (15030.0)
     }
    it should "return 20430 seconds for 'bla 5:40:30 h long'" in
     {
         parse("en", "Time", "bla 5:40:30 h long").get should equal (20430.0)
     }
    it should "return 20430 seconds for 'bla 5:40:30 super'" in
     {
         parse("en", "Time", "bla 5:40:30 super").get should equal (20430.0)
     }
    // These tests should fail:
    it should "return None for 'bla 5:40:30 seconds'" in
     {
         parse("en", "Time", "bla 5:40:30 seconds") should equal (None)
     }
    it should "return None for '2002-present'" in
     {
         parse("en", "Time", "2002-present") should equal (None)
     }

  
    /**
     * Tests for https://en.wikipedia.org/wiki/Template:Duration
     *
     * Named parameters:
     * {{Duration|h=1|m=22|s=34}} renders as: 1:22:34
     * {{Duration|m=74|s=32}} renders as: 74:32
     * {{Duration|m=2|s=34}} renders as: 2:34
     * {{Duration|h=1|s=34}} renders as: 1:00:34
     * {{Duration|h=1}} renders as: 1:00:00
     * {{Duration|h=0|m=22|s=34}} renders as: 0:22:34
     * {{Duration|h=0|m=0|s=34}} renders as: 0:00:34
     * {{Duration|h=1|m=22|s=34.5678}} renders as: 1:22:34.5678
     * {{Duration|h=1|m=22|s=3}} renders as: 1:22:03
     * {{Duration|h=1|m=2|s=34}} renders as: 1:02:34
     * {{Duration|h=1|m=2|s=3}} renders as: 1:02:03
     * {{Duration|h=1|m=2}} renders as: 1:02:00
     * {{Duration|s=34}} renders as: 0:34
     * {{Duration|m=0|s=34}} renders as: 0:34
     * {{Duration|h=1|m=0|s=34}} renders as: 1:00:34
     * {{Duration|h=1|m=22|s=0}} renders as: 1:22:00
     *
     * Numbered parameters:
     * {{Duration|1|22|34}} renders as: 1:22:34
     * {{Duration||74|32}} renders as: 74:32
     * {{Duration|1|2|34}} renders as: 1:02:34
     * {{Duration|1|2|3}} renders as: 1:02:03
     * {{Duration|0|22|34}} renders as: 0:22:34
     * {{Duration|0|0|34}} renders as: 0:00:34
     * {{Duration||0|34}} renders as: 0:34
     * {{Duration|||34}} renders as: 0:34
     * {{Duration|1|22|34.5678}} renders as: 1:22:34.5678
     * {{Duration||1|22}} renders as: 1:22
     * {{Duration|1|22}} renders as: 1:22:00
     * {{Duration|1|22|0}} renders as: 1:22:00
     * {{Duration|0|0|0}} renders as:
     * {{Duration|||}} renders as:
     *
     */

    // {{Duration|h=1|m=22|s=34}}
    // {{Duration|1|22|34}}
    "DurationParser" should "return 1 * 3600.0 + 22 * 60.0 + 34 seconds" in {
      val expected = Some(1 * 3600.0 + 22 * 60.0 + 34)
      parse("en", "Time", "{{Duration|h=1|m=22|s=34}}") should equal (expected)
      parse("en", "Time", "{{Duration|1|22|34}}") should equal (expected)
    }

    // {{Duration|m=74|s=32}}
    // {{Duration||74|32}}
    it should "return 74 * 60.0 + 32 seconds" in {
      val expected = Some(74 * 60.0 + 32)
      parse("en", "Time", "{{Duration|m=74|s=32}}") should equal (expected)
      parse("en", "Time", "{{Duration||74|32}}") should equal (expected)
    }

    // {{Duration|m=2|s=34}}
    it should "return 2 * 60 + 34 seconds" in {
      parse("en", "Time", "{{Duration|m=2|s=34}}") should equal (Some(2 * 60.0 + 34))
    }

    // {{Duration|h=1|s=34}}
    // {{Duration|h=1|m=0|s=34}}
    it should "return 3634.0 seconds" in {
      val expected = Some(3634.0)
      parse("en", "Time", "{{Duration|h=1|s=34}}") should equal (expected)
      parse("en", "Time", "{{Duration|h=1|m=0|s=34}}") should equal (expected)
    }

    // {{Duration|h=1}}
    it should "return 1 * 3600.0 seconds" in {
      parse("en", "Time", "{{Duration|h=1}}") should equal (Some(1 * 3600.0))
    }

    // {{Duration|h=0|m=22|s=34}}
    // {{Duration|0|22|34}}
    it should "return 22 * 60.0 + 34 seconds" in {
      parse("en", "Time", "{{Duration|h=0|m=22|s=34}}") should equal (Some(22 * 60.0 + 34))
      parse("en", "Time", "{{Duration|0|22|34}}") should equal (Some(22 * 60.0 + 34))
    }

    // {{Duration|h=1|m=22|s=34.5678}}
    // {{Duration|1|22|34.5678}}
    it should "return 1 * 3600.0 + 22 * 60.0 + 34.5678 seconds" in {
      parse("en", "Time", "{{Duration|h=1|m=22|s=34.5678}}") should equal (Some(1 * 3600.0 + 22 * 60.0 + 34.5678))
      parse("en", "Time", "{{Duration|1|22|34.5678}}") should equal (Some(1 * 3600.0 + 22 * 60.0 + 34.5678))
    }

    // {{Duration|h=1|m=22|s=3}}
    it should "return 1 * 3600.0 + 22 * 60.0 + 3 seconds" in {
      parse("en", "Time", "{{Duration|h=1|m=22|s=3}}") should equal (Some(1 * 3600.0 + 22 * 60.0 + 3))
    }

    // {{Duration|h=1|m=2|s=34}}
    // {{Duration|1|2|34}}
    it should "return 1 * 3600.0 + 2 * 60.0 + 34 seconds" in {
      val expected = Some(1 * 3600.0 + 2 * 60.0 + 34)
      parse("en", "Time", "{{Duration|h=1|m=2|s=34}}") should equal (expected)
      parse("en", "Time", "{{Duration|1|2|34}}") should equal (expected)
    }

    // {{Duration|h=1|m=2|s=3}}
    // {{Duration|1|2|3}}
    it should "return 1 * 3600.0 + 2 * 60.0 + 3 seconds" in {
      val expected = Some(1 * 3600.0 + 2 * 60.0 + 3)
      parse("en", "Time", "{{Duration|h=1|m=2|s=3}}") should equal (expected)
      parse("en", "Time", "{{Duration|1|2|3}}") should equal (expected)
    }

    // {{Duration|h=1|m=2}}
    it should "return 1 * 3600.0 + 2 * 60.0 seconds" in {
      parse("en", "Time", "{{Duration|h=1|m=2}}") should equal (Some(1 * 3600.0 + 2 * 60.0))
    }

    // {{Duration|s=34}}
    // {{Duration|m=0|s=34}}
    // {{Duration|h=0|m=0|s=34}}
    // {{Duration|0|0|34}}
    // {{Duration||0|34}}
    // {{Duration|||34}}
    it should "return 34 seconds" in {
      parse("en", "Time", "{{Duration|s=34}}") should equal (Some(34))
      parse("en", "Time", "{{Duration|m=0|s=34}}") should equal (Some(34))
      parse("en", "Time", "{{Duration|h=0|m=0|s=34}}") should equal (Some(34))
      parse("en", "Time", "{{Duration|0|0|34}}") should equal (Some(34))
      parse("en", "Time", "{{Duration||0|34}}") should equal (Some(34))
      parse("en", "Time", "{{Duration|||34}}") should equal (Some(34))
    }

    // {{Duration|h=1|m=22|s=0}}
    // {{Duration|1|22}}
    // {{Duration|1|22|0}}
    it should "return 1 * 3600.0 + 22 * 60.0 seconds" in {
      parse("en", "Time", "{{Duration|h=1|m=22|s=0}}") should equal (Some(3600.0 + 22 * 60.0))
      parse("en", "Time", "{{Duration|1|22}}") should equal (Some(3600.0 + 22 * 60.0))
      parse("en", "Time", "{{Duration|1|22|0}}") should equal (Some(3600.0 + 22 * 60.0))
    }

    // {{Duration||1|22}}
    it should "return 1 * 60.0 + 22 seconds" in {
      parse("en", "Time", "{{Duration||1|22}}") should equal (Some(1 * 60.0 + 22))
    }

    // {{Duration|0|0|0}}
    // {{Duration|||}}
    it should "return 0 seconds" in {
      parse("en", "Time", "{{Duration|0|0|0}}") should equal (Some(0))
      parse("en", "Time", "{{Duration|||}}") should equal (Some(0))
    }
     /**
     * Matcher to test if 2 values are approximately equal.
     */
    case class approximatelyEqualTo(r : Double) extends BeMatcher[Double]
    {
        val epsilon = 0.001

        def apply(l: Double) =
            MatchResult(
                compare(l, r),
                l + " is not approximately equal to " + r,
                l + " is approximately equal to " + r
            )

        private def compare(l : Double, r : Double) : Boolean =
        {
            scala.math.abs(l - r) < epsilon
        }
    }


    private val datatypes =  OntologyDatatypes.load().map(dt => (dt.name, dt)).toMap
    private val wikiParser = WikiParser.getInstance()

    private def parse(lang : String, datatypeName : String, input : String) : Option[Double] =
    {
        val context = new
        {
            def language : Language = Language(lang)
        }

        val durationParser = new DurationParser(context)

        val inputDatatype = datatypes(datatypeName)
        
        input.charAt(0) match
        {
          case '{' => {
            val page = new WikiPage(WikiTitle.parse("TestPage", context.language), input)

            wikiParser(page) match {
              case Some(n) => durationParser.parseToSeconds(n.children(0).asInstanceOf[TemplateNode], inputDatatype)
              case _ => None
            }
          }
          case _ => durationParser.parseToSeconds(input, inputDatatype)
        }

    }
}
