# Dates

## PDF Date Format

Dates in PDF are specified according to the following format:

`D:YYYYMMDDHHmmSSOHH'mm'`

where:

- `YYYY` is the year;

- `MM` is the month;

- `DD` is the day (01-31);

- `HH` is the hour (00-23);

- `mm` is the minute (00-59);

- `SS` is the second (00-59);

- `O` is the relationship of local time to Universal Time (UT), denoted
  by '+', '-' or 'Z';

- `HH` is the absolute value of the offset from UT in hours (00-23);

- `mm` is the absolute value of the offset from UT in minutes (00-59).

A contiguous prefix of the parts above can be used instead, for lower
accuracy dates. For example:

`D:2014` (2014)

`D:20140103` (3rd January 2014)

`D:201401031854-08'00'` (3rd January 2014, 6:54PM, US Pacific Standard
Time)

## XMP Metadata Date Format

These are the possible data formats for `-set-metadata-date`:

    YYYY
    YYYY-MM
    YYYY-MM-DD
    YYYY-MM-DDThh:mmTZD
    YYYY-MM-DDThh:mm:ssTZD

where:

-  `YYYY`   is the year
-  `MM`     is the month (01 = Jan)
-  `DD`     is the day of month (01 to 31)
-  `hh`     is the hour (00 to 23)
-  `mm`     is the minute (00 to 59)
-  `ss`     is the second (00 to 59)
-  `TZD`    is the time zone designator (`Z` or `+hh:mm` or `-hh::mm`)
