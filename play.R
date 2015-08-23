archive <- function() {
  # old stuff
  # Justify...1995 start?
  events_by_year <- dt[, nrow(.SD), by = year]
  fatalities_by_year <- dt[, sum(as.numeric(FATALITIES)), by = year]
  
  #Before 1993, only tornado, thunderstorm wind, and hail data were collected
  ue_before_1993 <- as.character(unique(dt[year < 1993]$EVTYPE))
  
  # Problematic PROPEXP values are isolated to a few years
  dt[, length(unique(PROPDMGEXP)), by = year]
  
  as.character(unique(dt[year <1993]$PROPDMGEXP))
  #[1] "K" "M" "" 
  
  as.character(unique(dt[year >1996]$PROPDMGEXP))
  #"K" ""  "M" "B" "0"
  
  #> as.character(unique(dt[year == 1993]$PROPDMGEXP))
  #[1] "B" ""  "K" "M" "?" "0" "6"
  #> as.character(unique(dt[year == 1994]$PROPDMGEXP))
  #[1] ""  "K" "M" "+" "0" "5" "h" "m" "4" "?"
  #> as.character(unique(dt[year == 1995]$PROPDMGEXP))
  #[1] ""  "B" "M" "K" "m" "+" "0" "5" "6" "?" "4" "2" "3" "7" "H" "-" "1" "8"
  
  # Can deal with offending values manually?
  #  > unique(dt$PROPDMGEXP)
  # [1] "K" "M" ""  "B" "m" "+" "0" "5" "6" "?" "4" "2" "3" "h" "7" "H" "-" "1" "8"
  
  View(dt[PROPDMGEXP == "+"])
}

cleanEv <- function(dirty) {
  # Reports summary data for all variables of interest for
  # a given subsection of evtype.
  
  print("FATALITIES")
  print(summary(dt[dirty]$FATALITIES))
  print("INJURIES")
  print(summary(dt[dirty]$INJURIES))
  print("PROPDMG")
  print(summary(dt[dirty]$PROPDMG))
  print("CROPDMG")
  print(summary(dt[dirty]$CROPDMG))
  
  invisible(dt[dirty])
  
}

autoClean <- function(dt) {
  evTypes <- as.character(unique(dt$EVTYPE))
  
  # Report variables of interest for each evType
  result.all <- lapply(evTypes,
    function(e) {
      dt.sub <- dt[EVTYPE == e]
      ev.summary <- data.table(
        "EVTYPE" = e,
        "nrow" = nrow(dt.sub),
        "FATALITIES" = sum(as.numeric(dt.sub$FATALITIES)),
        "INJURIES" = sum(dt.sub$INJURIES),
        "PROPDMG" = sum(dt.sub$PROPDMG),
        "CROPDMG" = sum(dt.sub$CROPDMG)
      )
      
      return(ev.summary)
    }
  )
  
  # Filter out ones that are all zero!
  result.nonzero <- lapply(result.all,
    function(e) {
      if(sum(e$FATALITIES, e$INJURIES, e$PROPDMG, e$CROPDMG) > 0) {
        return(e)
      } else {
        return(NA) 
      }
    }
  )
  
  result.nonzero <- result.nonzero[which(!is.na(result.nonzero))]
  
  result.nonzero.output <- do.call(rbind, result.nonzero)
  
  result.nonzero.output
  
}

fixUE <- function(ue) {
  fUE <- sapply(ue, handleEvName)
  uew <- data.table(before = ue, after = fUE)
  
  write.csv(uew[after != "AWYEA"], "aw.csv")
  uew
}

handleEvName <- function(e) {
  # Get to consistent cases first, in case.
  e <- toupper(e)
  
  canHandle <- toupper(readLines("evtype.txt"))
  canHandle <- paste("^", canHandle, "$", sep = "")
  
  # The easy stuff,  total match to file.
  if(grepl(paste(canHandle, collapse = "|"), e)) {
    # No change.
    return(e) 
  } 
  
  # Manual Fixes
  else if(grepl("TSTM WIND|THUNDERSTORM WIND", e)) {
    # Thunderstorm Wind
    return("AWYEA") 
  }
  
  else if(grepl("HURRICANE|TSUNAMI", e)) {
    # Hurricane/Typhoon
    return("AWYEA")
  }
  
  else if(grepl("TORNADO", e)) {
    # Tornado
    return("AWYEA")
  }
  
  else if(grepl("FLASH FLOOD", e)) {
    # Flash Flood
    return("AWYEA")
  }
  
  else if(grepl("HIGH WIND", e)) {
    # High Wind
    return("AWYEA")
  }
  
  else if(grepl("LIGHTING", e)) {
    # Lightning
    return("AWYEA")
  }
  
  else {
    return(e)
  }
  
}

play.read <- function() {
  # Read in data.
  f <- "repdata-data-StormData.csv"
  dt <<- as.data.table(read.csv(f,
    colClasses = c(
     "NULL", 
     "character",
     rep("NULL", 5),   
     "factor",         
     rep("NULL", 14),  
     "character",        
     "numeric",        
     "numeric",       
     "factor",     
     "numeric",       
     "factor",
     rep("NULL", 9) 
    )
  ))
  
  # Get year data
  dt$BGN_DATE <<- gsub(" .+", "", dt$BGN_DATE)
  dt$BGN_DATE <<- as.Date(dt$BGN_DATE, "%m/%d/%Y")
  dt$year <<- year(dt$BGN_DATE)
  
  # Now discard pre-1993 data.
  dt <<- dt[year >= 1993] 
  
  # Now add in proper factor levels
  dt$pexp <<- dt$PROPDMGEXP
  levels(dt$pexp) <<- c(
    "0", "0", "0", "0", 0:8, "9", "2", "2", "3", "6", "6"
  )
  dt$pexp <<- as.numeric(as.character(dt$pexp))
  
  dt$cexp <<- dt$CROPDMGEXP
  levels(dt$cexp) <<- c(
    "0", "0", "0", "2", "9", "3", "3", "6", "6"
  )
  dt$cexp <<- as.numeric(as.character(dt$cexp))
  
  # Now create the new rows for prop and crop
  dt$prop <<- dt$PROPDMG * 10^dt$pexp
  dt$crop <<- dt$CROPDMG * 10^dt$cexp
  
  # Now add the summed people measure
  dt$person <<- as.numeric(dt$FATALITIES) + dt$INJURIES
  
  # Now do some cleanup
  dt[
    , 
    `:=` (
      BGN_DATE = NULL,
      PROPDMG = NULL,
      CROPDMG = NULL,
      PROPDMGEXP = NULL,
      CROPDMGEXP = NULL,
      pexp = NULL,
      cexp = NULL
    )
  ]
  
  dt.sum <- dt[
    ,
    .(
      fatalities = sum(as.numeric(FATALITIES)),
      injuries = sum(INJURIES),
      person = sum(person),
      prop = sum(prop),
      crop = sum(crop),
      total = sum(prop + crop),
      nrow = nrow(.SD)
    ),
    by = EVTYPE
  ]
  
}

play.sum <- function(dt.sum) {
  require(data.table)
  
  totals <<- data.table(
    fatal = sum(dt.sum$fatalities),
    inj = sum(dt.sum$injuries),
    prop = sum(dt.sum$prop),
    crop = sum(dt.sum$crop),
    tot = sum(dt.sum$total),
    person = sum(dt.sum$person)
  )
  
  
  # fatalities
  m <- 1
  n <- 20
  sum(dt.sum[order(-fatalities)]$fatalities[m:n]) / totals$fatal 
  
  # injuries
  sum(dt.sum[order(-injuries)]$injuries[m:n]) / totals$inj 
  
  # person
  sum(dt.sum[order(-person)]$person[m:n]) / totals$person 
  
  # prop
  sum(dt.sum[order(-prop)]$prop[m:n]) / totals$prop 
  
  # crop
  sum(dt.sum[order(-crop)]$crop[m:n]) / totals$crop 
  
  # total
  sum(dt.sum[order(-total)]$total[m:n]) / totals$tot 
}

play.dt.evtype <- function(dt.sum) {
  # Generate new copies of levels
  dt.sum$ev.fix <- dt.sum$EVTYPE
  dt.sum$ev.general <- dt.sum$EVTYPE
  
  # Get the 497 "no damage" EVTYPES
  ev.noDamage <- dt.sum[person == 0 & total == 0]$EVTYPE

  ev.fixMap <- list(
    "X-ZERO" = as.character(ev.noDamage),
    "DROUGHT" = c(
      "DROUGHT/EXCESSIVE HEAT",
      "HEAT WAVE DROUGHT"
    ),
    "DEBRIS FLOW" = c(
      "Landslump",
      "LANDSLIDES",
      "LANDSLIDE",
      "MUDSLIDES",
      "MUD SLIDE",
      "MUD SLIDES",
      "MUDSLIDE",
      "ROCK SLIDE",
      "Mudslides",
      "Mudslide"
    ),
    "DENSE FOG" = c(
      "FOG"
    ),
    "COLD/WIND CHILL" = c(
      "COOL AND WET",
      "COLD AND WET CONDITIONS",
      "RECORD COLD",
      "FOG AND COLD TEMPERATURES",
      "HYPOTHERMIA",
      "COLD/WINDS",
      "Hypothermia/Exposure",
      "UNSEASONABLY COLD",
      "COLD WAVE",
      "LOW TEMPERATURE",
      "COLD WEATHER",
      "Unseasonable Cold",
      "Extended Cold",
      "Cold",
      "Cold Temperature",
      "COLD"
    ),
    "WINTER WEATHER" = c(
      "MIXED PRECIP",
      "Mixed Precipitation",
      "MIXED PRECIPITATION",
      "ICE",
      "SNOW",
      "SNOW/ICE",
      "HEAVY SNOWPACK",
      "BLOWING SNOW",
      "GLAZE",
      "RAIN/SNOW",
      "THUNDERSNOW",
      "SNOW/SLEET/FREEZING RAIN",
      "GLAZE ICE",
      "SNOW AND ICE",
      "ICE FLOES",
      "SNOW/COLD",
      "ICY ROADS",
      "SNOW FREEZING RAIN",
      "SNOW/SLEET",
      "SNOW/FREEZING RAIN",
      "ICE AND SNOW",
      "SNOW/ BITTER COLD",
      "SNOW ACCUMULATION",
      "SNOW/ ICE",
      "SNOW/BLOWING SNOW",
      "Snow",
      "blowing snow",
      "Glaze",
      "COLD AND SNOW",
      "BLACK ICE",
      "ICE ROADS",
      "LATE SEASON SNOW",
      "FALLING SNOW/ICE",
      "ICE ON ROAD",
      "WINTRY MIX",
      "LIGHT SNOW",
      "Wintry Mix",
      "Light snow",
      "Light Snow",
      "Light Snowfall",
      "WINTER WEATHER MIX",
      "WINTER WEATHER/MIX",
      "FREEZING DRIZZLE",
      "FREEZING RAIN/SNOW",
      "Freezing Rain",
      "Freezing Drizzle",
      "Freezing drizzle",
      "LIGHT FREEZING RAIN",
      "FREEZING RAIN",
      "HEAVY MIX"
    ),
    "WINTER STORM" = c(
      "ICE STORM",
      "SLEET/ICE STORM",
      "SNOW AND ICE STORM",
      "GLAZE/ICE STORM",
      "HEAVY SNOW/WINTER STORM",
      "SNOW/ICE STORM",
      "ICE/STRONG WINDS",
      "SNOW/HIGH WINDS",
      "HIGH WINDS/SNOW",
      "HEAVY SNOW AND STRONG WINDS",
      "HEAVY SNOW/HIGH WINDS & FLOOD",
      "WINTER STORM HIGH WINDS",
      "WINTER STORMS",
      "HEAVY SNOW AND HIGH WINDS",
      "HIGH WIND/HEAVY SNOW",
      "HEAVY SNOW/WIND"
      "HEAVY SNOW/FREEZING RAIN",
      "HEAVY RAIN/SNOW"
    ),
    "WILDFIRE" = c(
      "WILD FIRES",
      "WILD/FOREST FIRE",
      "GRASS FIRES",
      "FOREST FIRES",
      "WILDFIRES",
      "WILD/FOREST FIRES",
      "BRUSH FIRE"
    ),
    "TROPICAL STORM" = c(
      "TROPICAL STORM ALBERTO",
      "TROPICAL STORM GORDON",
      "TROPICAL STORM JERRY",
      "TROPICAL STORM DEAN"
    ),
    "TROPICAL DEPRESSION" = c(
      "Gradient wind",
      "GRADIENT WIND",
      "gradient wind"
    ),
    "TORNADO" = c(
      "FUNNEL CLOUD",
      "FUNNEL",
      "WATERSPOUT",
      "TORNADO F3",
      "TORNADO F0",
      "WATERSPOUT/TORNADO",
      "WATERSPOUT TORNADO",
      "WATERSPOUT-TORNADO",
      "WATERSPOUT-",
      "TORNADOES, TSTM WIND, HAIL",
      "COLD AIR TORNADO",
      "WATERSPOUT/ TORNADO",
      "DUST DEVIL WATERSPOUT",
      "TORNADO",
      "TORNDAO",
      "TORNADO F1",
      "TORNADO F2",
      "TORNADOES",
      "Whirlwind",
      "LANDSPOUT",
      "WHIRLWIND",
      "THUNDERSTORM WINDS/FUNNEL CLOU"
    ),
    "COASTAL FLOOD" = c(
      "Coastal Flood",
      "HEAVY SURF COASTAL FLOODING",
      "COASTAL FLOODING",
      "HIGH WINDS/COASTAL FLOOD",
      "Coastal Flooding",
      "Erosion/Cstl Flood",
      "COASTAL FLOODING/EROSION",
      "COASTAL  FLOODING/EROSION"
    ),
    "THUNDERSTORM WIND" = c(
      "GUSTNADO",
      " TSTM WIND (G45)",
      " TSTM WIND",
      "THUNDERSTORM WINDS",
      "THUNDERSTORM WINS",
      "THUNDERSTORM",
      "SEVERE THUNDERSTORM",
      "SEVERE THUNDERSTORMS",
      "SEVERE THUNDERSTORM WINDS",
      "THUNDERSTORMS WINDS",
      "THUNDERSTORMS",
      "THUNDERSTORM WINDSS",
      "LIGHTNING THUNDERSTORM WINDS",
      "LIGHTNING AND THUNDERSTORM WIN",
      "THUNDERSTORM WINDS53",
      "THUNDERSTORM WINDS 13",
      "TSTM WIND 55",
      "THUNDERTORM WINDS",
      "THUNDERSTORMS WIND",
      "THUNDERSTORM  WINDS",
      "TUNDERSTORM WIND",
      "THUNDERSTORM WIND/LIGHTNING",
      "THUNDERSTORM WIND G50",
      "THUNDERSTORM WIND G60",
      "THUNDERSTORM WINDS.",
      "THUNDERSTORM WIND G55",
      "THUNDERSTORM WINDS G60",
      "TSTM WIND G58",
      "THUNDERSTORM WIND 60 MPH",
      "THUNDERSTORM WIND 65MPH",
      "THUNDERSTORM WIND/ TREES",
      "THUNDERSTORM WIND/AWNING",
      "THUNDERSTORM WIND 98 MPH",
      "THUNDERSTORM WIND TREES",
      "THUNDERSTORM WINDS 63 MPH",
      "THUNDERSTORM WIND/ TREE",
      "THUNDERSTORM DAMAGE TO",
      "THUNDERSTORM WIND 65 MPH",
      "THUNDERSTORM WIND.",
      "THUNDERSTORM HAIL",
      "THUNDERSTORM WINDSHAIL",
      "THUDERSTORM WINDS",
      "THUNDERSTORM WINDS AND",
      "TSTM WIND DAMAGE",
      "THUNDERSTORM WIND G52",
      "THUNDERESTORM WINDS",
      "THUNDEERSTORM WINDS",
      "THUNERSTORM WINDS",
      "THUNDERSTORM WIND/HAIL",
      "THUNDERSTORMW",
      "TSTM WINDS",
      "TSTMW",
      "TSTM WIND 65)",
      "THUNDERSTORMWINDS",
      "THUNDERSTROM WIND",
      "Tstm Wind",
      "TSTM WIND (G45)",
      "TSTM WIND 40",
      "TSTM WIND 45",
      "TSTM WIND (41)",
      "TSTM WIND (G40)",
      "TSTM WIND",
      "TSTM WIND AND LIGHTNING",
      "TSTM WIND (G45)",
      "TSTM WIND  (G45)",
      "HIGH WIND (G40)",
      "TSTM WIND (G35)",
      "TSTM WIND G45",
      "THUNDERSTORM WIND (G40)",
      "THUNDERSTORM WINDS LIGHTNING",
      "THUNDERSTORM WINDS/HAIL",
      "THUNDERSTORM WINDS HAIL",
      "DRY MICROBURST",
      "MICROBURST",
      "DOWNBURST",
      "WET MICROBURST",
      "DRY MIRCOBURST WINDS",
      "MICROBURST WINDS",
      "TSTM WIND/HAIL",
      "Microburst",
      "Gustnado"
    ),
    "STRONG WIND" = c(
      "Strong Wind",
      "GUSTY WIND/HVY RAIN",
      "WIND",
      "WIND DAMAGE",
      "GUSTY WINDS",
      "WINDS",
      "Gusty wind/rain",
      "Gusty Winds",
      "GUSTY WIND",
      "Gusty winds",
      "NON-TSTM WIND",
      "NON TSTM WIND",
      "STRONG WINDS",
      "Wind",
      "Wind Damage",
      "Strong Winds",
      "NON-SEVERE WIND DAMAGE"
    ),
    "STORM SURGE/TIDE" = c(
      "STORM SURGE",
      "COASTAL STORM",
      "COASTAL SURGE",
      "COASTAL EROSION",
      "Coastal Storm",
      "COASTALSTORM"
    ),
    "SLEET" = c(
      "FREEZING RAIN/SLEET"
    ),
    "RIP CURRENT" = c(
      "RIP CURRENTS/HEAVY SURF",
      "RIP CURRENTS"
    ),
    "MARINE THUNDERSTORM" = c(
      "MARINE TSTM WIND",
      "MARINE THUNDERSTORM WIND"
    ),
    "MARINE HIGH WIND" = c(
      "HIGH WIND/SEAS",
      "HIGH WIND AND SEAS"
    ),
    "BLIZZARD" = c(
      "HEAVY SNOW/BLIZZARD",
      "BLIZZARD/WINTER STORM",
      "GROUND BLIZZARD",
      "HIGH WIND/BLIZZARD"
    ),
    "LIGHTNING" = c(
      "LIGHTING",
      "LIGHTNING INJURY",
      "LIGNTNING",
      "LIGHTNING.",
      "LIGHTNING FIRE",
      "LIGHTNING  WAUSEON"
    ),
    "LAKE-EFFECT SNOW" = c(
      "HEAVY LAKE SNOW",
      "LAKE EFFECT SNOW",
      "Lake Effect Snow"
    ),
    "HURRICANE/TYPHOON" = c(
      "HURRICANE OPAL/HIGH WINDS",
      "HURRICANE ERIN",
      "HURRICANE OPAL",
      "HURRICANE",
      "HURRICANE-GENERATED SWELLS",
      "HURRICANE EMILY",
      "HURRICANE GORDON",
      "HURRICANE FELIX",
      "Hurricane Edouard",
      "TYPHOON"
    ),
    "HIGH WIND" = c(
      "WIND STORM",
      "STORM FORCE WINDS",
      "HIGH WINDS/COLD",
      "HIGH WINDS",
      "HIGH WIND DAMAGE",
      "HIGH WINDS/",
      "HIGH  WINDS",
      "HIGH WIND 48"
    ),
    "HIGH SURF" = c(
      "High Surf",
      "HIGH SURF ADVISORY",
      "   HIGH SURF ADVISORY",
      "Beach Erosion",
      "HIGH TIDES",
      "HEAVY SURF",
      "Heavy Rain/High Surf",
      "Heavy surf and wind",
      "HIGH WAVES",
      "ROUGH SURF",
      "Heavy Surf",
      "HIGH SWELLS",
      "HIGH SURF ADVISORY",
      "HAZARDOUS SURF",
      "ASTRONOMICAL HIGH TIDE",
      "HEAVY SURF/HIGH SURF"
    ),
    "HEAVY SNOW" = c(
      "SNOW SQUALLS",
      "SNOW SQUALL",
      "RECORD SNOW",
      "Snow Squalls",
      "EXCESSIVE SNOW",
      "SNOW AND HEAVY SNOW",
      "SNOW/HEAVY SNOW",
      "HEAVY SNOW/ICE",
      "HEAVY SNOW SQUALLS",
      "HEAVY SNOW/SQUALLS",
      "HEAVY SNOW-SQUALLS",
      "Heavy snow shower"
    ),
    "HEAVY RAIN" = c(
      "RECORD RAINFALL",
      "HEAVY PRECIPITATION",
      "HEAVY RAINS",
      "RAINSTORM",
      "HEAVY RAIN/SEVERE WEATHER",
      "RAIN",
      "HVY RAIN",
      "RAIN/WIND",
      "EXCESSIVE RAINFALL",
      "Torrential Rainfall",
      "UNSEASONAL RAIN",
      "LIGHTNING AND HEAVY RAIN",
      "HEAVY RAIN/LIGHTNING",
      "LIGHTNING/HEAVY RAIN",
      "HIGH WINDS HEAVY RAINS",
      "HIGH WINDS/HEAVY RAIN",
      "HEAVY SHOWER"
    ),
    "HEAT" = c(
      "HEAT WAVE",
      "UNSEASONABLY WARM",
      "RECORD HEAT",
      "HEAT WAVES",
      "UNSEASONABLY WARM AND DRY",
      "Heat Wave",
      "WARM WEATHER"
    ),
    "AVALANCHE" = c(
      "AVALANCE",
      "HEAVY SNOW/BLIZZARD/AVALANCHE"
    ),
    "HAIL" = c(
      "GUSTY WIND/HAIL",
      "HAIL 75",
      "SMALL HAIL",
      "HAIL 0.75",
      "HAIL/WINDS",
      "HAIL/WIND",
      "WIND/HAIL",
      "HAIL 175",
      "HAIL 100",
      "HAIL 150",
      "HAIL 075",
      "HAIL 125",
      "HAIL 200",
      "HAIL DAMAGE",
      "HAIL 275",
      "HAIL 450",
      "HAILSTORM"
    ),
    "FROST/FREEZE" = c(
      "Frost/Freeze",
      "FREEZE",
      "DAMAGING FREEZE",
      "FROST",
      "AGRICULTURAL FREEZE",
      "FROST\\FREEZE",
      "HARD FREEZE",
      "Freeze",
      "Damaging Freeze",
      "Early Frost"
    ),
    "FLOOD" = c(
      "HIGH WATER",
      "MUD SLIDES URBAN FLOODING",
      "FLOODING",
      "FLOODING/HEAVY RAIN",
      "FLOOD/RAIN/WINDS",
      "MINOR FLOODING",
      "FLOODS",
      "FLOOD/RIVER FLOOD",
      "Tidal Flooding",
      "River Flooding",
      "TIDAL FLOODING",
      "SNOWMELT FLOODING",
      "THUNDERSTORM WINDS/ FLOOD",
      "Ice jam flood (minor",
      "URBAN SMALL",
      "BREAKUP FLOODING",
      "RIVER FLOOD",
      "URBAN FLOODING",
      "URBAN FLOOD",
      "URBAN/SMALL STREAM FLOOD",
      "RURAL FLOOD",
      "MAJOR FLOOD",
      "SMALL STREAM FLOOD",
      "LAKE FLOOD",
      "URBAN AND SMALL STREAM FLOODIN",
      "RIVER AND STREAM FLOOD",
      "RIVER FLOODING",
      "URBAN/SMALL STREAM",
      "HEAVY RAIN AND FLOOD",
      "HEAVY RAINS/FLOODING",
      "THUNDERSTORM WINDS/FLOODING",
      "FLOOD & HEAVY RAIN",
      "URBAN FLOODS",
      "HEAVY RAIN/SMALL STREAM URBAN",
      "URBAN/SML STREAM FLD",
      "URBAN AND SMALL"
    ),
    "FLASH FLOOD" = c(
      " FLASH FLOOD",
      "FLASH FLOOD",
      "RAPIDLY RISING WATER",
      "DAM BREAK",
      "ICE JAM",
      "ICE STORM/FLASH FLOOD",
      "FLASH FLOODING",
      "FLASH FLOODS",
      "FLASH FLOOD WINDS",
      "FLASH FLOOD/",
      "FLASH FLOOD - HEAVY RAIN",
      "FLASH FLOOD/ STREET",
      "FLOOD FLASH",
      "FLOOD/FLASH",
      "FLASH FLOODING/FLOOD",
      "FLASH FLOOD/LANDSLIDE",
      "FLASH FLOOD LANDSLIDES",
      "FLASH FLOOD",
      "FLASH FLOODING/THUNDERSTORM WI",
      "FLOOD/FLASH FLOOD",
      "FLASH FLOOD/FLOOD",
      "FLASH FLOOD FROM ICE JAMS",
      "FLOOD/FLASHFLOOD",
      "ICE JAM FLOODING",
      "FLOOD/FLASH/FLOOD"
    ),
    "EXTREME COLD/WIND CHILL" = c(
      "EXTREME COLD",
      "EXTREME WIND CHILL",
      "EXTREME WINDCHILL",
      "Extreme Cold",
      "HYPOTHERMIA/EXPOSURE",
      "HYPERTHERMIA/EXPOSURE"
    ),
    "EXCESSIVE HEAT" = c(
      "EXTREME HEAT",
      "RECORD/EXCESSIVE HEAT"
    ),
    "DUST STORM" = c(
      "BLOWING DUST",
      "DUST STORM/HIGH WINDS"
    ),
    "DUST DEVIL" = c(
      "Dust Devil"
    ),
    "X-OTHER" = c(
      "SEVERE TURBULENCE",
      "APACHE COUNTY",
      "HIGH",
      "EXCESSIVE WETNESS",
      "OTHER",
      "?",
      "Other",
      "DROWNING"
    ),
    "X-MARINE" = c(
      "MARINE MISHAP",
      "HIGH SEAS",
      "HEAVY SEAS",
      "HEAVY SWELLS",
      "Marine Accident",
      "Freezing Spray",
      "WIND AND WAVE",
      "ROUGH SEAS",
      "ROGUE WAVE"
    ),
    "VOLCANIC ASH" = "Volcanic Ash",
    "SEICHE" = "SEICHE",
    "TSUNAMI" = "TSUNAMI",
    "FREEZING FOG" = "FREEZING FOG",
    "MARINE HAIL" = "MARINE HAIL",
    "LAKESHORE FLOOD" = "LAKESHORE FLOOD",
    "MARINE STRONG WIND" = "MARINE STRONG WIND",
    "ASTRONOMICAL LOW TIDE" = "ASTRONOMICAL LOW TIDE",
    "DENSE SMOKE" = "DENSE SMOKE"
  )
  
  levels(dt.sum$ev.fix) <- ev.fixMap
  
  #for(i in length(ev.fixMap)) {
   # fix <- ev.fixMap[[i]]$fix
    #orig <- ev.fixMap[[i]]$orig
    
    # If any of the levels appear in "orig", then fix it.
    #if(sum(levels(dt.sum$ev.fix) %in% orig) >0 ){
     # levels(dt.sum$ev.fix)[which(levels(dt.sum$ev.fix) %in% orig)] <- fix
    #}
    
  #}
  
  # Make everything uppercase
  #levels(dt.sum$ev.fix) <- toupper(levels(dt.sum$ev.fix))
  
  dt.sum <<- dt.sum
 
  my.sum <<- dt.sum[
    ,
    .(
      fatalities = sum(fatalities),
      injuries = sum(injuries),
      person = sum(person),
      prop = sum(prop),
      crop = sum(crop),
      total = sum(prop + crop),
      nrow = nrow(.SD)
    ),
    by = ev.fix
  ]
  
}

play.plot <- function(my.sum) {
  require(ggplot2)
  require(reshape2)
  
  my.fatal <- my.sum[order(-fatalities), .(fatalities = fatalities, evtype = ev.fix)]
  plot.fatal <- rbind(my.fatal[1:20], 
    list(fatalities = sum(my.fatal$fatalities[21:nrow(my.fatal)]), evtype = "Other")
  )
    
  ggplot(plot.fatal, aes(x = evtype, y = fatalities, colour = evtype)) +
    geom_bar(stat="identity", size=3)                    
    
  my.person <- my.sum[order(-person), 
                      .(fatalities = fatalities,
                        injuries = injuries,
                        person = person,
                        evtype = ev.fix)]
  
  my.fatalities <- my.sum[order(-fatalities), 
                      .(fatalities = fatalities,
                        injuries = injuries,
                        person = person,
                        evtype = ev.fix)]
  
  my.injuries <- my.sum[order(-injuries),
                      .(fatalities = fatalities,
                        injuries = injuries,
                        person = person,
                        evtype = ev.fix)]
  
  m <- 20
  end <- nrow(my.person)
  plot.person <- rbind(my.person[1:m],
                       list(
                          fatalities = sum(my.person$fatalities[(m+1):end]),
                          injuries = sum(my.person$injuries[(m+1):end]),
                          person = sum(my.person$person[(m+1):end]),
                          evtype = "Other"
                       )
  )
  
  spec.person <- my.person[1:m, .(evtype = evtype, variable = "person", value = person)]
  spec.person <- rbind(spec.person,
                       list(
                          evtype = "Other",
                          variable = "person",
                          value = sum(my.person$person[(m+1):end])
                       )
  )
  
    
  spec.fatalities <- my.person[1:m, .(evtype = evtype, variable = "fatalities", value = fatalities)]
  spec.fatalities <- rbind(spec.fatalities,
                       list(
                         evtype = "Other",
                         variable = "fatalities",
                         value = sum(my.fatalities$fatalities[(m+1):end])
                       )
  )
  ggplot(plot.person, 
    aes(x = evtype, y = person)) + 
    geom_bar(stat = "identity")
    
  melt.person <- melt(my.person, id.vars = "evtype")
  melt.person$evtype2 <- factor(melt.person$evtype, levels = my.person[order(-person)]$evtype)
  ggplot(melt.person, aes(x = evtype2, y = value + 1, fill = evtype2)) +
    geom_bar(stat = "identity") + coord_flip() +
    facet_grid(.~ variable) +
    theme(legend.position="none") +
    scale_y_log10(breaks = c(10, 100, 500, 100, 1000, 2500, 10000, 25000))
  

  my.damage
}