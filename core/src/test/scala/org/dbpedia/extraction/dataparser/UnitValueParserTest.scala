package org.dbpedia.extraction.dataparser

import org.dbpedia.extraction.mappings.Redirects
import org.dbpedia.extraction.wikiparser._
import org.dbpedia.extraction.sources.{WikiPage,MemorySource}
import org.dbpedia.extraction.util.Language
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.matchers.{MatchResult, BeMatcher}
import scala.math._
import org.dbpedia.extraction.ontology.datatypes.InconvertibleUnitDatatype
import org.dbpedia.extraction.ontology.{Ontology, OntologyDatatypes}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class UnitValueParserTest extends FlatSpec with Matchers
{
    // Length - Positive Tests - Input is valid
    "UnitValueParser" should "return Length: 10 m in 10 m" in
    {
        parse("en", "Length", "10m") should be (approximatelyEqualTo(Some(10.0)))
    }

    it should "return Length: 500 m in .5 km" in {
      parse("en", "Length", ".5 km") should be (approximatelyEqualTo(Some(500.0)))
      parse("de", "Length", ",5 km") should be (approximatelyEqualTo(Some(500.0)))
    }

    it should "return Length: 10 m in 10 metres" in
    {
        parse("en", "Length", "10metres") should equal (Some(10.0))
    }
    it should "return Length: 1 m in 1 metre" in
    {
        parse("en", "Length", "1 metre") should equal (Some(1.0))
    }
    it should "return Length: 1.61 m in {{convert|1610|mm|in|1|abbr=on}}" in
    {
        parse("en", "Length", "{{convert|1610|mm|in|1|abbr=on}}") should equal (Some(1.61))
    }
    it should "return Length: 19120 m in {{conversion|19.12|km|ft|2}}" in
    {
        parse("fr", "Length", "{{conversion|19.12|km|m|ft|2}}") should equal  (Some(19120))
    }
    it should "return Length: 19120 m in {{convertir|19.12|km|ft|2}}" in
    {
        parse("es", "Length", "{{convertir|19.12|km|m|ft|2}}") should equal  (Some(19120))
    }

    /**
    // FIXME: Not supported
    it should "return Length: 0.297 m in 210 × 297&nbsp;mm" in
    {
        parse("en", "Length", "210 × 297&nbsp;mm") should equal (Some(0.297))
    }
    */

    /**
    // FIXME: These templates are not supported! Do they make any sense?!?
    it should "return Length: 2181 m in {{Infobox mountain | elevation_m = 2181 }}" in
    {
        parse("en", "Length", "{{Infobox mountain | elevation_m = 2181 }}") should equal (Some(2181))
    }
    it should "return Length: 471 m in {{Infobox mountain | prominence_m = 471 }}" in
    {
        parse("en", "Length", "{{Infobox mountain | prominence_m = 471 }}") should equal (Some(471))
    }
    it should "return Length: 4810.9 m in {{Geobox|Range|highest_elevation=4810.9 }}" in
    {
        parse("en", "Length", "{{Geobox|Range|highest_elevation=4810.9 }}") should equal (Some(4810.9))
    }
    it should "return Length: 3944502.14 m in {{Infobox road | length_mi = 2451 }}" in
    {
        parse("en", "Length", "{{Infobox road | length_mi = 2451 }}") should equal (Some(3944502.14))
    }
    */

    it should "return Length: 0.112 m in {{convert|112|mm|in|abbr=on}}" in
    {
        parse("en", "Length", "{{convert|112|mm|in|abbr=on}}") should equal (Some(0.112))
    }
    it should "return Length: 2.8448 m in {{convert|112|in|mm|abbr=on}}" in
    {
        parse("en", "Length", "{{convert|112|in|mm|abbr=on}}") should be (approximatelyEqualTo(Some(2.8448)))
    }
    it should "return Length: 1.9812 m in 6 ft 6 in" in
    {
        parse("en", "Length", "6 ft 6 in") should be (approximatelyEqualTo(Some(1.9812)))
    }
    it should "return Length: 1.9812 m in 6 feet 6 inch" in
    {
        parse("en", "Length", "6 feet 6 inch") should be (approximatelyEqualTo(Some(1.9812)))
    }
    it should "return Length: 1.9812 m in 6' 6\"" in
    {
        parse("en", "Length", "6' 6''") should be (approximatelyEqualTo(Some(1.9812)))
    }
    it should "return Length: 1.8288 m in 6'" in
    {
        parse("en", "Length", "6'") should be (approximatelyEqualTo(Some(1.8288)))
    }
    it should "return Length: 2.1589 m in The 7 foot 1 inch Chamberlain " in
    {
        parse("en", "Length", "The 7 foot 1 inch Chamberlain ") should be  (approximatelyEqualTo(Some(2.1589)))
    }
    it should "return Length: 0.254 m in 10 in" in
    {
        parse("en", "Length", "10 in") should be  (approximatelyEqualTo(Some(0.254)))
    }
    it should "return Length: 4.826 m in 15ft 10in" in
    {
        parse("en", "Length", "15ft 10in") should be  (approximatelyEqualTo(Some(4.826)))
    }
    it should "return Length: 1.9812 m in {{height|ft=6|in=6}}dgjhgj34" in
    {
        parse("en", "Length", "{{height|ft=6|in=6}}dgjhgj34") should be  (approximatelyEqualTo(Some(1.9812)))
    }
    it should "return Length: 72420.48 m in {{convert|45|mi}} [[Boxing the compass|N]]" in
    {
        parse("en", "Length", "{{convert|45|mi}} [[Boxing the compass|N]]") should be  (approximatelyEqualTo(Some(72420.48)))
    }
    it should "return Length: 0.012 m in 12mm (13in)" in
    {
        parse("en", "Length", "12mm (13in)") should equal (Some(0.012))
    }
    it should "return Length: 3678000 m in {{M|3678|k|m}}" in
    {
        parse("it", "Length", "{{M|3678|k|m}}") should equal (Some(3678000.0))
    }
    it should "return Length: 3678000 m in {{M|3,678|e=6||m}}" in
    {
        parse("it", "Length", "{{M|3,678|e=6||m}}") should equal (Some(3678000.0))
    }

    /**
    // FIXME: Not supported.
    "UnitValueParser" should "return Length(The '''22 [[nanometre|nanometer]]''' node )" in
    {
        parse("en", "Length", "The '''22 [[nanometre|nanometer]]''' node ") should equal (Some(0.000000022))
    }
    */
    it should "return Length: 0.0000007 m in longwards of 0.7 µm" in
    {
        parse("en", "Length", "longwards of 0.7 µm") should equal (Some(0.0000007))
    }

    // https://en.wikipedia.org/wiki/Template:Height
    /**
     * {{height|m=1.77|precision=0}}	1.77 m (5 ft 10 in)
     * {{height|m=1.77|frac=16}}	1.77 m (5 ft 9 11⁄16 in)
     * {{height|ft=6}}	6 ft 0 in (1.83 m)
     * {{height|ft=6|in=1}}	6 ft 1 in (1.85 m)
     * {{height|ft=6|in=1|precision=1}}	6 ft 1 in (1.9 m)
     * {{height|ft=5|in=7+1/2}}	5 ft 7 1⁄2 in (1.71 m)
     * {{height|ft=5|in=7+1/2|precision=1}}	5 ft 7 1⁄2 in (1.7 m)
     * {{height|in=10}}	0 ft 10 in (0.25 m)
     * {{height|m=1.72|precision=0}}	1.72 m (5 ft 8 in)
     * {{height|ft=6|in=2|abbr=mos}}	6 feet 2 inches (1.88 m)
     * {{height|ft=6|in=2}}	6 ft 2 in (1.88 m)
     * {{height|ft=5|in=7+1/2|precision=3}}	5 ft 7 1⁄2 in (1.715 m)
     */

    // {{height|m=1.77|precision=0}}
    // {{height|m=1.77|frac=16}}
    it should "return Length: 1.77 m with {{height|m=1.77|precision=0}}" in {
      parse("en", "Length", "{{height|m=1.77|precision=0}}") should equal (Some(1.77))
    }

    it should "return Length: 1.77 m with {{height|m=1.77|frac=16}}" in {
      parse("en", "Length", "{{height|m=1.77|frac=16}}") should equal (Some(1.77))
    }

    // {{height|ft=6}}
    it should "return Length: 1.8288 m with {{height|ft=6}}" in {
      parse("en", "Length", "{{height|ft=6}}") should equal (Some(1.8288))
    }

    // {{height|ft=6|in=1}}
    // {{height|ft=6|in=1|precision=1}}
    it should "return Length: 1.85 m with {{height|ft=6|in=1}}" in {
      parse("en", "Length", "{{height|ft=6|in=1}}") should be (Some(1.8542))
    }

    it should "return Length: 1.85 m with {{height|ft=6|in=1|precision=1}}" in {
      parse("en", "Length", "{{height|ft=6|in=1|precision=1}}") should equal (Some(1.8542))
    }

    // {{height|in=10}}
    it should "return Length: 0.254 m with {{height|in=10}}" in {
      parse("en", "Length", "{{height|in=10}}") should equal (Some(0.254))
    }

    // {{height|m=1.72|precision=0}}
    it should "return Length: 1.72 m with {{height|m=1.72|precision=0}}" in {
      parse("en", "Length", "{{height|m=1.72|precision=0}}") should equal (Some(1.72))
    }

    // {{height|ft=6|in=2|abbr=mos}}
    // {{height|ft=6|in=2}}
    it should "return Length: 1.88 m with {{height|ft=6|in=2|abbr=mos}}" in {
      parse("en", "Length", "{{height|ft=6|in=2|abbr=mos}}") should equal (Some(1.8796))
    }

    it should "return Length: 1.88 m with {{height|ft=6|in=2}}" in {
      parse("en", "Length", "{{height|ft=6|in=2}}") should equal (Some(1.8796))
    }

    // {{altura|m=2.01}}
    it should "return Length: 2.01 m with {{altura|m=2.01}}" in {
      parse("es", "Length", "{{altura|m=2.01}}") should equal (Some(2.01))
    }

    //Area - Positive Tests - Input is valid
    // Removed!
    // https://en.wikipedia.org/wiki/Template:Pop_density_mi2_to_km2
    /** it should "return Area: 919445779.0 m² in {{Pop density mi2 to km2|355|precision=0|abbr=yes}}" in
    {
        parse("en", "Area", "{{Pop density mi2 to km2|355|precision=0|abbr=yes}}") should be (approximatelyEqualTo(Some(919445779.0)))
    }
    */
    it should "return Area: 0.00001 m² in 10 mm²" in
    {
        parse("en", "Area", "10 mm²") should equal (Some(0.00001))
    }
    it should "return Area: 0.001 m² in 10 cm²" in
    {
        parse("en", "Area", "10 cm²") should equal (Some(0.001))
    }
    it should "return Area: 111483.648 m² in 1,200,000 ft²" in
    {
        parse("en", "Area", "1,200,000 ft²") should be (approximatelyEqualTo(Some(111483.648)))
    }
    it should "return Area: 21300000 m² in 21.30 km²" in
    {
        parse("en", "Area", "21.30 km²") should equal (Some(21300000))
        parse("en", "Area", "21.30 km\u00B2") should equal (Some(21300000))
        parse("en", "Area", "21.30 Km\u00B2") should equal (Some(21300000))
    }
    it should "return Area: 500000 m² in 0.5 km²" in
    {
      parse("en", "Area", ".5 km²") should equal (Some(500000))
      parse("de", "Area", ",5 km²") should equal (Some(500000))
    }
    it should "return Area: 21.30 m² in 21.30 m²" in
    {
        parse("en", "Area", "21.30 m²") should equal (Some(21.30))
    }
    it should "return Area: 5.5166746743E7 m² in 21.30 mi²" in
    {
        parse("en", "Area", "21.30 mi²") should be (approximatelyEqualTo(Some(5.5166746743E7)))
    }
    it should "return Area: 213000 m² in 21.30 ha" in
    {
        parse("en", "Area", "21.30 ha") should equal (Some(213000))
    }
    it should "return Area: 1.978834752 m² in 21.30 Sq ft" in
    {
        parse("en", "Area", "21.30 Sq ft") should be (approximatelyEqualTo(Some(1.978834752)))
    }
    it should "return Area: 17.809512768 m² in 21.30 Square yard" in
    {
        parse("en", "Area", "21.30 Square yard") should be (approximatelyEqualTo(Some(17.809512768)))
    }
    it should "return Area: 1394142.04 m² in 344.50 acres (1.39 km²)" in
    {
        parse("en", "Area", "344.50 acres (1.39 km²") should be (approximatelyEqualTo(Some(1394142.04)))
    }
    it should "return Area: 77000000 m² in {{km2 to mi2 | 77 | abbr=yes}}" in
    {
        parse("en", "Area", "{{km2 to mi2 | 77 | abbr=yes}}") should be (approximatelyEqualTo(Some(77000000.0)))
    }
    it should "return Area: 5200000 m² in {{unité|325.92|km|2}}" in
    {
        parse("fr", "Area", "{{unité|325.92|km|2}}") should equal (Some(325920000.0))
    }

    //Volume - Positive Tests - Input is valid
   it should "return Volume: 10000000000 m³ in 10 km³" in
    {
        parse("en", "Volume", "10 km³") should equal (Some(10000000000.0))
    }
    it should "return Volume: 500000000 m³ in .5 km³" in
    {
      parse("en", "Volume", ".5 km³") should equal (Some(500000000.0))
      parse("de", "Volume", ",5 km³") should equal (Some(500000000.0))
    }
    it should "return Volume: 10 m³ in 10 m³" in
    {
        parse("en", "Volume", "10 m³") should equal (Some(10))
    }
    it should "return Volume: 0.00001 m³ in 10 cm3" in
    {
        parse("en", "Volume", "10 cm3") should equal (Some(0.00001))
    }
    it should "return Volume: 0.01 m³ in 10 cubic decimetre" in
    {
        parse("en", "Volume", "10 cubic decimetre") should equal (Some(0.01))
    }
    // TODO: fluid ounces are not supported
    /*
    it should "return Volume: 0.0000355 m³ in 12 U.S. fl oz" in
    {
        parse("en", "Volume", "12 U.S. fl oz") should be (approximatelyEqualTo(Some(0.0000355)))
    }
    it should "return Volume: 0.0000355 m³ in 12.5 imp fl oz" in
    {
        parse("en", "Volume", "12.5 imp fl oz") should be (approximatelyEqualTo(Some(0.0000355)))
    }
    */
    // TODO: plurals are not supported
    /*
    it should "return Volume: 158.9873 m³ in is 42 US gallons" in
    {
        parse("en", "Volume", " is 42 US gallons") should be (approximatelyEqualTo(Some(158.9873)))
    }
    */
    it should "return Volume: 2316672 m³ in {{convert|612000000|USgal|m3|abbr=on}}" in
    {
        parse("en", "Volume", "{{convert|612000000|USgal|m3|abbr=on}}") should be (approximatelyEqualTo(Some(2316672.0)))
    }

    //Speed - Positive Tests - Input is valid
    it should "return Speed: 90 km/h in {{unité|90|km||h|-1}}" in
    {
        parse("fr", "Speed", "{{unité|90|km||h|-1}}") should equal (Some(90.0))
    }

  
    //Power/Energy - Positive Tests - Input is valid
    it should "return Power: 0.01 W in 10 mW" in
    {
        parse("en", "Power", "10 mW") should equal (Some(0.01))
    }
    it should "return Power: 3.094e9 W in 3.094E3 MW" in
    {
        parse("fr", "Power", "3.094E+03 MW") should equal (Some(3094000000.0))
    }
    it should "return Power: 3.094e9 W in {{unité|3.094|e=3|MW}}" in
    {
        parse("fr", "Power", "{{unité|3.094|e=3|MW}}") should equal (Some(3094000000.0))
    }
    it should "return Energy: 80e12 * 3600 J in {{convert|80|TWh|abbr=on|lk=on}}" in
    {
        parse("en", "Energy", "{{convert|80|TWh|abbr=on|lk=on}}") should be  (approximatelyEqualTo(Some(80e12 * 3600.0)))
    }

    // Ratio - Positive Tests - Input is valid
    it should "return Ratio: 0.01 in 1%" in
    {
        parse("en", "Ratio", "1%") should equal (Some(0.01))
    }
   
    //Time - Positive Tests - Input is valid
    "UnitValueParser" should "return Time: 432000 s in 5 Days" in
    {
        parse("en", "Time", "5 Days") should equal (Some(432000))
    }
    // TODO: Valid? This is very unusual
    /**
    it should "return Time: 7439 s in 2h 03:59" in
    {
        parse("en", "Time", "2h 03:59") should equal (Some(7439))
    }
    */
    it should "return Time: 7830 s in under 2h 10'30\"" in
    {
        parse("en", "Time", "under 2h 10'30\"") should equal (Some(7830))
    }
    it should "return Time: 8125 s in in 2 hours 15 minutes and 25 seconds" in
    {
        parse("en", "Time", "in 2 hours 15 minutes and 25 seconds") should equal (Some(8125))
    }
    it should "return Time: 0.01 s in A latency of 10 milliseconds " in
    {
        parse("en", "Time", "A latency of 10 milliseconds ") should equal (Some(0.01))
    }
    it should "return Time: 216000 s in 2.5 d" in
    {
        parse("en", "Time", "2.5 d") should equal (Some(216000))
    }
    it should "return Time: 1800 s in 0.5 hours" in
    {
        parse("en", "Time", ".5 hours") should equal (Some(1800))
        parse("de", "Time", ",5 hours") should equal (Some(1800))
    }
    it should "return Time: -120 s in -2 min" in
    {
        parse("en", "Time", "-2 min") should equal (Some(-120))
    }
    it should "return Time: 6125 s in asdf:sdag 1:42:05 dsfg" in
    {
        parse("en", "Time", "asdf:sdag 1:42:05 dsfg") should equal (Some(6125))
    }
    it should "return Time: 9300 s in 2:35 h" in
    {
        parse("en", "Time", "2:35 h") should equal (Some(9300))
    }
    it should "return Time: 155 s in 2:35 min" in
    {
        parse("en", "Time", "2:35 min") should equal (Some(155))
    }

    //Time - Positive Tests - Input is valid
    "UnitValueParser" should "return Currency: 23.6E9 $ in 23,6 Mrd USD" in
    {
        parse("fr", "Currency", "23,6 Mrd USD") should equal (Some(23600000000.0))
    }
    it should "return Currency: 1.275E9 $ in 1,275 milliard de $" in
    {
        parse("fr", "Currency", "1,275 milliard de $") should equal (Some(1275000000.0))
    }
    it should "return Currency: 2.245E11 $ in 224,5 milliards $" in
    {
        parse("fr", "Currency", "224,5 milliards $") should equal (Some(224500000000.0))
    }
    it should "return Currency: 1.275E9 € in 1,275 milliard d'€" in
    {
        parse("fr", "Currency", "1,275 milliard d'€") should equal (Some(1275000000.0))
    }
    it should "return Currency: 2.113E9 $ in 2&nbsp;113 millions de [[Dollar américain|$ US]]" in
    {
        parse("fr", "Currency", "2 113 millions de [[Dollar américain|$ US]]") should equal (Some(2113000000.0))
    } 
    it should "return Currency: 26.743E9 $ in US$ 26&nbsp;743&nbsp;millones" in
    {
        parse("es", "Currency", "US$ 26&nbsp;743&nbsp;millones") should equal (Some(26743000000.0))
    }
    it should "return Currency: 3.820464E12 $ in [[US$]] 3&nbsp;820&nbsp;464 mill." in
    {
        parse("es", "Currency", "[[US$]] 3&nbsp;820&nbsp;464 mill.") should equal (Some(3820464000000.0))
    }
    it should "return Currency: 159869000 R$ in {{formatnum:159 869 000}}" in
    {
        parse("pt", "brazilianReal", "{{formatnum:159 869 000}}") should be (approximatelyEqualTo(Some(159869000.0)))
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
    "UnitValueParser" should "return 1 * 3600.0 + 22 * 60.0 + 34 seconds" in {
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

    /*
      it should "return Time:  s in 2:35 min" in
      {
          parse("en", "Time", "2:35 min") should equal (Some(155))
      }
    */

    it should "return Mass: 12500 g in 12,5 kg" in
    {
        parse("en", "Mass", "12.5 kg") should equal (Some(12500))
        parse("de", "Mass", "12,5 kg") should equal (Some(12500))
    }
    it should "return Mass: 3210 g in {{convert|3.21|kg|lb|0}}" in
    {
        parse("en", "Mass", "{{convert|3.21|kg|lb|0}}") should equal (Some(3210))
    }
    it should "return Mass: 1456.031 g in ({{convert|3.21|lb|kg|0}}" in
    {
        parse("en", "Mass", "{{convert|3.21|lb|kg|0}}") should be (approximatelyEqualTo(Some(1456.031)))
    }
    // TODO: Would be cool to support fractions
    /**
    it should "return Mass: 3200 g in approximately 7 ½ lbs.(3.2&nbsp;kg)" in
    {
        parse("en", "Mass", "approximately 7 ½ lbs.(3.2&nbsp;kg") should equal (Some(3200))
    }
    */
    
  
    // Directly setting Unit not just Dimension
    
    "UnitValueParser" should "return 10 m in 10 metre" in
    {
        parse("en", "metre", "10") should be (approximatelyEqualTo(Some(10.0)))
    }

    it should "return 1.495978707E11 m in 1 AU" in
    {
      parse("en", "astronomicalUnit", "1") should be (Some(149597870700.0))
    }
    it should "return 1.495978707E10 m in 0.1 AU" in
    {
      parse("en", "astronomicalUnit", "0.1") should be (Some(14959787070.0))
      parse("en", "astronomicalUnit", ".1") should be (Some(14959787070.0))
      parse("de", "astronomicalUnit", ",1") should be (Some(14959787070.0))
    }
    it should "return 1.1892681609948355E11 in .7949766633909954 AU " in
    {
      // https://github.com/dbpedia/extraction-framework/issues/71
      parse("en", "astronomicalUnit", ".7949766633909954") should be (Some(1.1892681609948355E11))
    }
    
    // Negative Tests - Actual unit does not march dimension
    
    "UnitValueParser" should "return Nothing in Length: 12cl" in
    {
        parse("en", "Length", "12cl") should equal (None)
    }
    
    "UnitValueParser" should "return Nothing in Power: 10MWh" in
    {
        parse("en", "Power", "10MWh") should equal (None)
    }

  

    /**
     * Matcher to test if 2 values are approximately equal.
     */
    case class approximatelyEqualTo(r : Option[Double]) extends BeMatcher[Option[Double]]
    {
        val epsilon = 0.1

        def apply(l: Option[Double]) =
            MatchResult(
                compare(l, r),
                l + " is not approximately equal to " + r,
                l + " is approximately equal to " + r
            )

        private def compare(l : Option[Double], r : Option[Double]) : Boolean =
        {
            (l, r) match
            {
                case (None, None) => true
                case (Some(_), None) => false
                case (None, Some(_)) => false
                case (Some(vl), Some(vr)) => compare(vl, vr)
            }
        }

        private def compare(l : Double, r : Double) : Boolean =
        {
            abs(l - r) < epsilon
        }
    }

    private val wikiParser = WikiParser.getInstance()
    private val datatypes =  OntologyDatatypes.load().map(dt => (dt.name, dt)).toMap

    private def parse(language : String, datatypeName : String, input : String) : Option[Double] =
    {
        val lang = Language(language)
        val red = new Redirects(Map())

        val context = new
        {
            // def ontology : Ontology = throw new Exception("please test without requiring the ontology")
            // Let's fake an ontology with datatypes loaded from OntologyDatatypes - for Time tests
            def ontology : Ontology = new Ontology(Map(), Map(), datatypes , Map(), Map(), Map())
            def language : Language = lang
            def redirects : Redirects = red
        }

        val datatype = datatypes(datatypeName)
        val unitValueParser = new UnitValueParser(context, datatype, false)
        val page = new WikiPage(WikiTitle.parse("TestPage", lang), input)

        wikiParser(page) match {
          case Some(n) =>  unitValueParser.parse(n).map{
              case (value, dt) if (dt.isInstanceOf[InconvertibleUnitDatatype]) => value
              case (value, dt) => dt.toStandardUnit(value)
          }
          case None => None
        }
    }
}