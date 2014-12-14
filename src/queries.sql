SELECT GLOBALEVENTID, MonthYear, EventCode, QuadClass, ActionGeo_CountryCode, ActionGeo_Lat, ActionGeo_Long
FROM [gdelt-bq:full.events]
WHERE (Actor2Type1Code = "REB" or Actor2Type1Code = "SEP") and (MonthYear > 201403) and (ActionGeo_CountryCode IN ("AG", "AO", "BY", "CD", "CF", "CG", "CT", "EG", "ER", "ET", "IV", "KE", "LI", "LY", "MZ", "NI", "RW", "SF", "SG", "SL", "SO", "SU", "TZ", "UG", "ZA", "ZI"))
ORDER BY MonthYear
