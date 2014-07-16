package org.dbpedia.extraction.dataparser

import org.dbpedia.extraction.util.WikiUtil
import org.dbpedia.extraction.wikiparser._
import org.dbpedia.extraction.util.Language
import org.dbpedia.extraction.mappings.Redirects
import scala.util.matching.Regex.Match

/**
 * Parses a human-readable character string from a node. 
 */
class LangStringParser ( extractionContext : {
                           def language : Language
                           def redirects : Redirects } ) extends DataParser
{
    private val smallTagRegex = """<small[^>]*>\(?(.*?)\)?<\/small>""".r

    private val language = extractionContext.language

    override def parse(node : Node) : Option[(String, Language)] =
    {
        node match
        {
            case _ : TemplateNode => catchTemplate(node.asInstanceOf[TemplateNode])
            case _ => parseString(node, language)
        }
    }
  
    private def catchTemplate(node : TemplateNode) : Option[(String, Language)] =
    {
        extractionContext.redirects.resolve(node.title).decoded.toLowerCase match
        {
            //case "lang" | "rtl-lang" => Some(node.property(2), language)
            case _ => None
        }
    }
    
    private def parseString(node : Node, lang : Language) : Option[(String, Language)] =
    {
      StringParser.parse(node) match
      {
        case Some(text) => Some(text, lang)
        case _ => None
      }
    }
  
}
