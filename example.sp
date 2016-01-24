#!/usr/bin/env sproc

# comments start with #

# overall syntax is like shell or other command-based language

# look in nginx logs for some backend statistics
fields (nginx-log defaultformat) | take http.errors | take (@.remote_addr not in internal_nets()) | stats [Perc95, Perc98] (sum @.upstream_response_time)

fields (nginx-log defaultformat) | take {@.remote_addr not in (internal_nets)}


# select some records in stream, select particular text region inside that records and change it inplace. Then print only selected fields

fields (nginx-log defaultformat) | take http.ok && {@.uri ~= "/shortname"} | withf @.uri {s "^/shortname" "/longname/version"} | withf @.user-agent {get Platform (parse_UA)}


# define a function

func parse_UA (ua string) (val int) :> int {
    set a 42
    returns a
}

# define a streaming function
sfunc chunk(size int) {
    set buf (list)
    loops {
        # collect
        if (len buf) == size {
            emit buf
            set buf (list)
        } else {
            append buf @
        }
        # then emit
    }
}

# parallel processing: splitting, duplicating, joining streams

lines | split {grep "HIT"} @cache_hit @cache_miss

from @cache_hit | calculate_response_time | tee @counter Hit | stats [Perc95] 
from @cache_miss | calculate_response_time | tee @counter Miss | histogram @.uri

from
