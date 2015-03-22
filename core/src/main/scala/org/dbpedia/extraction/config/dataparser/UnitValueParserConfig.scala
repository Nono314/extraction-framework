package org.dbpedia.extraction.config.dataparser


object UnitValueParserConfig
{

    //specifies for a template name (lower-cased) the property keys of value and unit
    val convertTemplateMap = Map(
        "en" -> Map(
            // http://en.wikipedia.org/wiki/Template:Convert
            // {{convert|original_value|original_unit|conversion_unit|round_to|...}}
            "convert"  -> Map ("value" -> "1", "unit"-> "2"),
            // http://en.wikipedia.org/wiki/Template:Unit_weight
            // {{unit weight|unit|value|round_to}}
            "unit weight"  -> Map ("value" -> "2", "unit"-> "1"),
            // http://en.wikipedia.org/wiki/Template:Auto_in
            // {{Auto in|value|round_to}}
            "auto in"  -> Map ("value" -> "1", "cstUnit"-> "inch"), // doesn't exist anymore, remove?
            // http://en.wikipedia.org/wiki/Template:Km_to_mi
            // {{km to mi|value|...}}
            "km to mi"  -> Map ("value" -> "1", "cstUnit"-> "kilometre"), // doesn't exist anymore, remove?
            // http://en.wikipedia.org/wiki/Template:Km2_to_mi2
            // {{km2 to mi2|value|...}}
            "km2 to mi2"  -> Map ("value" -> "1", "cstUnit"-> "square kilometre"), // doesn't exist anymore, remove?
            // http://en.wikipedia.org/wiki/Template:Pop_density_km2_to_mi2
            // {{Pop density km2 to mi2|value|...}}
            // {{PD km2 to mi2|value|...}}
            "pop density km2 to mi2"  -> Map ("value" -> "1", "cstUnit"-> "square kilometre"), // doesn't exist anymore, remove?
            "pd km2 to mi2"  -> Map ("value" -> "1", "cstUnit"-> "square kilometre"), // doesn't exist anymore, remove?
            // http://en.wikipedia.org/wiki/Template:Ft_to_m
            // {{ft to m|value|...}}
            // {{ftom|value|...}}
            "ft to m"  -> Map ("value" -> "1", "cstUnit"-> "foot"), // doesn't exist anymore, remove?
            "ftom"  -> Map ("value" -> "1", "cstUnit"-> "foot") // doesn't exist anymore, remove?
        ),
        "de" -> Map(
            // http://de.wikipedia.org/wiki/Vorlage:Maß
            // {{Einheitenumrechnung|original_value|unit_converion|round_to}}
            "einheitenumrechnung"  -> Map ("value" -> "1", "unit"-> "2"/*, "unitExtract" -> {x : String => x.substring(0, x.indexOf('2')-1)}*/),
            // http://de.wikipedia.org/wiki/Vorlage:Maß
            // {{Maß|original_value|original_unit|lower_threshold|smaller_unit|upper_threshold|larger_unit|round=round_to|...}}
            "maß"  -> Map ("value" -> "1", "unit"-> "2")
        ),
        "es" -> Map(
            // http://es.wikipedia.org/wiki/Plantilla:Convertir
            // {{convertir|original_value|original_unit|conversion_unit|round_to|...}}
            "convertir"  -> Map ("value" -> "1", "unit"-> "2")
        ),
        "fr" -> Map(
            // http://fr.wikipedia.org/wiki/Modèle:Conversion
            // {{conversion|original_value|original_unit|conversion_unit|round_to|...}}
            "conversion"  -> Map ("value" -> "1", "unit"-> "2")
        ),
        "it" -> Map(
            // http://it.wikipedia.org/wiki/Template:Converti
            // {{converti|original_value|original_unit|conversion_unit|round_to|...}}
            "converti"  -> Map ("value" -> "1", "unit"-> "2")
        ),
        "pt" -> Map(
            // http://pt.wikipedia.org/wiki/Predefinição:Convert
            // {{convert|original_value|original_unit|conversion_unit|round_to|...}}
            "convert"  -> Map ("value" -> "1", "unit"-> "2"),
            // http://pt.wikipedia.org/wiki/Predefinição:Converter
            // {{converter|original_value|original_unit|conversion_unit|round_to|...}}
            "converter"  -> Map ("value" -> "1", "unit"-> "2")
        )
    )

    // templates in this map are expected to have properties with units as key
    // their values should belong to the same dimension, they will be converted to the standard unit and summed up
    val measurementTemplateMap = Map(
      "en" -> Map(
        // http://en.wikipedia.org/wiki/Template:Height
        // {{height|m=1.77|precision=0}}
        // {{height|ft=6|in=1}}
        "height" -> Set("precision", "frac", "abbr", "wiki") // property names to ignore
      ),
      "es" -> Map(
        // http://es.wikipedia.org/wiki/Plantilla:Altura
        // {{altura|m=2.01}}
        "altura" -> Set[String](),
        // http://es.wikipedia.org/wiki/Plantilla:Peso
        // {{peso|kg=50}}
        "peso" -> Set[String]()
      ),
      "fr" -> Map(
        // http://fr.wikipedia.org/wiki/Modèle:Taille
        // {{taille|m=2.01}}
        "taille" -> Set[String](),
        // http://fr.wikipedia.org/wiki/Modèle:Poids
        // {{poids|kg=50}}
        "poids" -> Set[String]()
      ),
      "pt" -> Map(
        // http://pt.wikipedia.org/wiki/Predefinição:Altura
        // {{altura|m=2.01}}
        "altura" -> Set("precision", "frac", "abbr", "wiki") // property names to ignore
      ),
      "tr" -> Map(
        // http://tr.wikipedia.org/wiki/%C5%9Eablon:Boy
        // {{altura|m=2.01}}
        "boy" -> Set[String]()
      )
    )
    
    val durationMap = Map(
      // https://en.wikipedia.org/wiki/Template:Duration
      // {{duration|h=1|m=20|s=32}}
      // {{duration|m=20|s=32}}
      // {{duration|1|20|32}}
      // {{duration||20|32}}
      "en" -> "duration",
      "pt" -> "duração"
    )
}
