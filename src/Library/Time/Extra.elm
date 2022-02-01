module Library.Time.Extra exposing (CanonicalTimeOfDay(..), fromDate)

import Date exposing (Date)
import Time
import Time.Extra as Time


type CanonicalTimeOfDay
    = StartOfDay
    | EndOfDay


fromDate : CanonicalTimeOfDay -> Time.Zone -> Date -> Time.Posix
fromDate canonicalTimeOfDay zone date =
    case canonicalTimeOfDay of
        StartOfDay ->
            Time.partsToPosix Time.utc
                { year = Date.year date
                , month = Date.month date
                , day = Date.day date
                , hour = 0
                , minute = 0
                , second = 0
                , millisecond = 0
                }

        EndOfDay ->
            Time.partsToPosix Time.utc
                { year = Date.year date
                , month = Date.month date
                , day = Date.day date
                , hour = 23
                , minute = 59
                , second = 59
                , millisecond = 999
                }
