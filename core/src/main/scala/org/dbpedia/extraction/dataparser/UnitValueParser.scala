package org.dbpedia.extraction.dataparser

import org.dbpedia.extraction.ontology.datatypes.{Datatype, DimensionDatatype, UnitDatatype}

import org.dbpedia.extraction.wikiparser._
import java.text.ParseException
import java.util.logging.{Level, Logger}
import java.util.regex.Pattern
import org.dbpedia.extraction.ontology.Ontology
import org.dbpedia.extraction.util.Language
import org.dbpedia.extraction.mappings.Redirects
//import java.lang.Double
import org.dbpedia.extraction.config.dataparser.{DataParserConfig, UnitValueParserConfig}
import scala.language.reflectiveCalls

class UnitValueParser( extractionContext : {
                           def ontology : Ontology
                           def language : Language
                           def redirects : Redirects },
                        inputDatatype : Datatype,
                        strict : Boolean = false,
                        multiplicationFactor : Double = 1.0) extends DataParser
{
    private val logger = Logger.getLogger(getClass.getName)

    private val parserUtils = new ParserUtils(extractionContext)

    private val durationParser = new DurationParser(extractionContext)

    private val language = extractionContext.language.wikiCode

    override val splitPropertyNodeRegex = if (DataParserConfig.splitPropertyNodeRegexUnitValue.contains(language))
                                            DataParserConfig.splitPropertyNodeRegexUnitValue.get(language).get
                                          else DataParserConfig.splitPropertyNodeRegexUnitValue.get("en").get
    
    private val convertTemplates = UnitValueParserConfig.convertTemplateMap.getOrElse(language, UnitValueParserConfig.convertTemplateMap("en"))
    private val measurementTemplates = UnitValueParserConfig.measurementTemplateMap.getOrElse(language, UnitValueParserConfig.measurementTemplateMap("en"))
    private val unitTemplates = UnitValueParserConfig.unitTemplateMap.getOrElse(language, UnitValueParserConfig.unitTemplateMap("en"))
    private val durationTemplate = UnitValueParserConfig.durationMap.getOrElse(language, UnitValueParserConfig.durationMap("en"))
    
    private val prefix = if(strict) """\s*""" else """[\D]*?"""

    private val postfix = if(strict) """\s*""" else ".*"
    
    private val unitRegexLabels = inputDatatype match
    {
        case dt : DimensionDatatype => dt.unitLabels.toList.sortWith((a,b) => a.length > b.length).map(Pattern.quote).mkString("|")
        case dt : UnitDatatype => dt.dimension.unitLabels.toList.sortWith((a,b) => a.length > b.length).map(Pattern.quote).mkString("|")
        case dt => throw new IllegalArgumentException("Invalid datatype: " + dt)
    }

    // Allow leading decimal separator, e.g. .0254 = 0.0254
    // See https://github.com/dbpedia/extraction-framework/issues/71
    private val ValueRegex = ("""(?iu)""" + prefix + """(-?""" + parserUtils.decimalSeparatorsRegex + """?[0-9]+(?:[""" + parserUtils.groupingSeparatorsRegex + """ ][0-9]{3})*(?:""" + parserUtils.decimalSeparatorsRegex + """[0-9]+)?)""" + postfix).r

    private val UnitRegex = ("""(?iu)""" + """(?<!\w)(""" + unitRegexLabels + """)(?!/)(?!\\)(?!\w)(?!\d)""").r
    
    /** Merging strings with feet and inches: 'x ft y in' and convert them into centimetres */
    private val UnitValueRegex_a = ("""(?iu)""" + prefix + """(-?[0-9]+)\040*(?:ft|feet|foot|\047|\054|\140)\040*([0-9]+)\040*(?:in\b|inch\b|inches\b|\047\047|\054\054|\140\140\042)""" + postfix).r
    
    /** Catches number and unit: e.q. 1,120,500.55 km */
    // Allow leading decimal separator, e.g. .0254 = 0.0254
    // See https://github.com/dbpedia/extraction-framework/issues/71
    private val UnitValueRegex_b = ("""(?iu)""" + prefix + """(?<!-)(-?""" + parserUtils.decimalSeparatorsRegex + """?[0-9]+(?:[""" + parserUtils.groupingSeparatorsRegex + """ ][0-9]{3})*(?:""" + parserUtils.decimalSeparatorsRegex + """[0-9]+)?)(?:&nbsp;)*\040*\(?\[?\[?(""" + unitRegexLabels +
                                    """)(?!/)(?!\\)(?!\w)""" + postfix).r
    
    /** If different units are present, e.g.: 10 mi. (16.0934 km); the first will be returned */
    //TODO remove?
    // Allow leading decimal separator, e.g. .0254 = 0.0254
    // See https://github.com/dbpedia/extraction-framework/issues/71
    private val UnitValueRegex_c = ("""(?iu)""" + prefix + """(?<!-)(-?""" + parserUtils.decimalSeparatorsRegex + """?[0-9]+(?:[""" + parserUtils.groupingSeparatorsRegex + """ ][0-9]{3})*(?:""" + parserUtils.decimalSeparatorsRegex + """[0-9]+)?)(?:&nbsp;)*\040*\(?\[?\[?(""" + unitRegexLabels +
                                    """)[\s]*\([\s]*(?:[0-9]+(?:""" + parserUtils.decimalSeparatorsRegex + """[0-9]+)?)[\s]*(?:""" + unitRegexLabels + """)[\s]*\)[\s]*""" + postfix).r
                                   
    private val PrefixUnitValueRegex = ("""(?iu)""" + prefix + """(""" + unitRegexLabels + """)\]?\]?\040*(?<!-)([\-0-9]+(?:""" + parserUtils.groupingSeparatorsRegex + """[0-9]{3})*(?:""" + parserUtils.decimalSeparatorsRegex + """[0-9]+)?)""" + postfix).r

    override def parse(node : Node) : Option[(Double, UnitDatatype)] =
    {
        val errors = if(logger.isLoggable(Level.FINE)) Some(new ParsingErrors()) else None

        for(result <- catchTemplates(node, errors))
        {
            return Some(result)
        }

        for(parseResult <- StringParser.parse(node))
        {
            val text = parserUtils.convertLargeNumbers(parseResult)

            inputDatatype match
            {
                case dt : DimensionDatatype if (dt.name == "Time") => for(duration <- catchDuration(text)) return Some(duration)
                case dt : UnitDatatype if (dt.dimension.name == "Time") => for(duration <- catchDuration(text)) return Some(duration)
                case _ =>
            }

            catchUnitValue(text, errors) match
            {
                case Some(result) => return Some(result)
                case None =>
                {
                    //No unit value found
                    if(inputDatatype.isInstanceOf[UnitDatatype])
                    {
                        for( value <- catchValue(text);
                             result <- generateOutput(value, None, errors) )
                        {
                            return Some(result)
                        }
                    }
                    else
                    {
                        errors.foreach(_.add("Could not find any unit value in '" + text + "'"))
                    }
                }
            }
        }

        for(e <- errors)
        {
            logger.fine("Could not extract " + inputDatatype.name + " value from " + node + " on page " + node.root.title + " line " + node.line + ".\n" + e)
        }

        None
    }

    /**
     * This Method parse property templates like {{convert|...}
     */
    private def catchTemplates(node : Node, errors : Option[ParsingErrors]) : Option[(Double, UnitDatatype)] =
    {
        // If the node is not a TemplateNode run catchTemplates() for all childs
        if(!node.isInstanceOf[TemplateNode])
        {
            if(!strict)
            {
                for (child <- node.children;
                     result <- catchTemplates(child, errors) )
                {
                    return Some(result)
                }

                return None;
            }
            else
            {
                node.children match
                {
                    case (child : TemplateNode) :: Nil => return catchTemplates(child, errors)
                    case _ => return None
                }
            }
        }

        val templateNode = node.asInstanceOf[TemplateNode]
        val templateName = extractionContext.redirects.resolve(templateNode.title).decoded


        val childrenChilds = for(child <- node.children) yield
            { for(childrenChild @ TextNode(_, _)<- child.children) yield childrenChild }
        
        ///////////////////////////////////////////////////////////////////////////////////////
        // Start of template parsing
        ///////////////////////////////////////////////////////////////////////////////////////
 
        var value : Option[String] = None
        var unit : Option[String] = None
        
        // TODO {{convert|3.21|m|cm}} and other occurences of two units help finding the dimension
        for(currentTemplate <- convertTemplates.get(templateName))
        {
            var valueNum  = currentTemplate.getOrElse("value", "1")
            var unitNum = currentTemplate.get("unit")
            var unitVal = currentTemplate.get("cstUnit")

            for (valueProperty <- templateNode.property(valueNum))
            {
                val value = UnitValueParser.getPropertyValue(Some(valueProperty), "0")
                unit = unitNum match
                {
                    case Some(un) => templateNode.property(un).get.children.collect{case TextNode(text, _) => text}.headOption
                    case None => unitVal
                }

                return generateOutput(value.replaceAll("\\.", ""+parserUtils.defaultDecimalSeparator), unit, errors)
            }
        }
    
        if (unitTemplates.contains(templateName))
        {
            unit = Some(( 2 to 7 map { x => UnitValueParser.getPropertyValue(templateNode.property(""+x), "") }).foldRight("") { 
              (i, a) =>  a match
              {
                  case "-1" => "/" + i
                  case _ => i + a
              }
            })
            for (valueProperty <- templateNode.property("1"))
            {
                var value = UnitValueParser.getPropertyValue(Some(valueProperty), "0")
                for (exp <- templateNode.property("e"))
                {
                    value = (value.toDouble * scala.math.pow(10.0, UnitValueParser.getPropertyValue(Some(exp), "0").toDouble)).toString
                }
                return generateOutput(value, unit, errors)
            }
        }

        // TODO: {{height|ft=5|in=7+1/2}}
        for(templateExclusions <- measurementTemplates.get(templateName))
        {
            val results = templateNode.keySet.filterNot(templateExclusions.contains(_)) flatMap
            {
                x => generateOutput(UnitValueParser.getPropertyValue(templateNode.property(x), "0"), Some(x), errors) 
            }
            
            val res = results.foldLeft((0.0, results.head._2))
            {
              (a, i) => a match
              {
                case (value, unit) if (unit == i._2) => (value+i._1, unit)
                case (value, unit) if (unit.dimension == i._2.dimension) => (unit.toStandardUnit(value) + i._2.toStandardUnit(i._1), unit.dimension.standardUnit.get)
                case _ => a
              }
            }
            
            if (res._1 != 0.0)
            {
                return Some(res)
            }
        }
        
        // Parameters are optional and their default value is 0
        if (templateName == durationTemplate)
        {
            val defaultValue = PropertyNode("", List(TextNode("0", 0)), 0)

            val hours = templateNode.property("h").getOrElse(templateNode.property("1").getOrElse(defaultValue))
            val minutes = templateNode.property("m").getOrElse(templateNode.property("2").getOrElse(defaultValue))
            val seconds = templateNode.property("s").getOrElse(templateNode.property("3").getOrElse(defaultValue))

            val h = hours.children.collect { case TextNode(t, _) => t }.headOption.getOrElse("0").toDouble
            val m = minutes.children.collect { case TextNode(t, _ ) => t }.headOption.getOrElse("0").toDouble
            val s = seconds.children.collect { case TextNode(t, _) => t}.headOption.getOrElse("0").toDouble

            value = Some((h * 3600.0 + m * 60.0 + s).toString)
            unit = Some("second")
        }
        // If there is no mapping defined for the template -> return null and log it
        else
        {
            errors.foreach(_.add("Unknown template: \"" + templateName + "\""))
            return None
        }
        ///////////////////////////////////////////////////////////////////////////////////////
        // End of template parsing
        ///////////////////////////////////////////////////////////////////////////////////////
        
        // If there is a mapping but the parsing falied -> return None
        if(value.isEmpty)
        {
            return None
        }
        
        generateOutput(value.get, unit, errors)
    }

    private def catchValue(input : String) : Option[String] =
    {
        input match
        {
            case ValueRegex(value) => Some(value)
            case _ => None
        }
    }

    private def catchDuration(input : String) : Option[(Double, UnitDatatype)] =
    {
        durationParser.parseToSeconds(input, inputDatatype) match
        {
            case Some(result) => Some((result, extractionContext.ontology.datatypes("second").asInstanceOf[UnitDatatype]))
            case None => None
        }
    }
    
    private def catchUnit(input : String) : Option[String] =
    {
        UnitRegex.findFirstIn(input)
    }
    
    /**
     * Returns unit and value for an Object
     * string with feet and inches will be converted in centimetre
     * 1 in = 2.54 cm
     * 1 ft = 30.48 cm
     *
     * The value and Unit of the passed value will be returned in an Array
     *
     * @param   string  $input  text
     * @return  array   the value at offset[0] and a UnitDataType object at offset[1].
     */
    private def catchUnitValue(input : String, errors : Option[ParsingErrors]) : Option[(Double, UnitDatatype)] =
    {
        val inputDimension = inputDatatype match
        {
            case dt : UnitDatatype => dt.dimension
            case dt : DimensionDatatype => dt
        }

        val catchPrefixedUnit = inputDimension.name == "Currency"

        input match
        {
            case UnitValueRegex_a(feet, inch) =>
            {
                try
                {
                    val ftToCm = feet.toDouble * 30.48
                    val inToCm = inch.toDouble * 2.54

                    generateOutput((ftToCm + inToCm).toString, Some("centimetre"), errors)
                }
                catch
                {
                    case _ : NumberFormatException => None
                }
            }
            case UnitValueRegex_b(value, unit) => generateOutput(value, Some(unit), errors)
            case UnitValueRegex_c(value, unit) => generateOutput(value, Some(unit), errors)
            case PrefixUnitValueRegex(unit, value) if catchPrefixedUnit => generateOutput(value, Some(unit), errors)
            case _ => None
        }
    }
    
    /**
     * Creates the output tuple, the number at 0, the unit at 1.
     */
    private def generateOutput(valueString : String, unitString : Option[String] = None, errors : Option[ParsingErrors]) : Option[(Double, UnitDatatype)] =
    {
        val value = 
        {
            try
            {
                parserUtils.parse(valueString).doubleValue * multiplicationFactor
            }
            catch
            {
                case ex : ParseException =>
                {
                    errors.foreach(_.add("Could not find number in '" + valueString + "'"))
                    return None
                }
            }
        }

        //Determine the datatype of the value
        val valueUnit = unitString match
        {
            // The unit is explicitly provided
            case Some(unitName) => inputDatatype match
            {
                //first match case-sensitive so that MW matches and is not equivalent to mW
                case inputUnit : UnitDatatype => inputUnit.dimension.unit(unitName) match
                {
                    case Some(unit) => unit
                    //only then case-insensitive to catch possible case mismatches such as Km i/o km
                    case None => inputUnit.dimension.unit(unitName.toLowerCase) match
                    {
                        case Some(unit) => unit
                        case None =>
                        {
                            errors.foreach(_.add("Given unit '" + unitName + "' not found"))
                            return None
                        }
                    }
                }
                //first match case-sensitive so that MW matches and is not equivalent to mW
                case inputDimension : DimensionDatatype => inputDimension.unit(unitName) match
                {
                    case Some(unit) => unit
                    //only then case-insensitive to catch possible case mismatches such as Km i/o km
                    case None => inputDimension.unit(unitName.toLowerCase) match
                    {
                        case Some(unit) => unit
                        case None =>
                        {
                            errors.foreach(_.add("Given unit '" + unitName + "' not found in dimension " + inputDimension))
                            return None
                        }
                    }
                }
                case _ => throw new Exception("Unexpected input datatype")
            }
            //No unit is explicitly provided
            case None => inputDatatype match
            {
                case inputUnit : UnitDatatype => inputUnit
                case _ =>
                {
                    errors.foreach(_.add("Value '" + valueString + "' found without any unit"))
                    return None
                }
            }
        }

        Some(value, valueUnit)
    }
}

private object UnitValueParser
{
    private def getPropertyValue(node : Option[PropertyNode], default:String) : String =
    {
        node.getOrElse(PropertyNode("", List(TextNode(default, 0)), 0)).children
            .collect{case TextNode(text, _) => text}.headOption.getOrElse(default)
    }
}