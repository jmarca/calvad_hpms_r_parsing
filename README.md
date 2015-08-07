# Read HPMS data

Older years were easier to read in using perl and then JS.  Starting
with 2010 or so, the HPMS data gets stranger, and it is easier to use
R and the various utilities available from dplyr and tidyr

At the moment, I'm focusing on 2012.  The strategy is to split out
numeric and text type values, and then recombine when it comes time to
write to the database, or perhaps not recombine at all but instead
stick to a set of relational tables to keep track of the data.

One item that is not yet dealt with is that the segment start and end
points are somehow being interpreted as a cascade or a layered thing.
So for example:

```
 Route_ID Begin_Point End_Point  AADT OWNERSHIP F_SYSTEM FACILITY_TYPE THROUGH_LANES
    18001       62.08     62.94    NA         4       NA            NA            NA
    18001       62.08     62.94    NA        NA       NA            NA            NA
    18001       62.08     63.59 24637        NA        3             2            NA
    18001       62.94     63.44    NA         2       NA            NA             2
    18001       62.94     63.44    NA        NA       NA            NA            NA
    18001       63.44     63.59    NA         4       NA            NA            NA
    18001       63.44     63.59    NA         4       NA            NA             2
    18001       63.59     64.10    NA         4       NA            NA             4
    18001       63.59     64.10    NA         4       NA            NA            NA
    18001       63.59     64.10 35250        NA        3             2             2

```

If you notice, the entire section, from 62.08 through 64.10 has an
AADT value.  However, several sections have NA assigned because they
are "under" the wider spans 62.08 to 63.59, and then 63.59 to
64.10. Within these spans, characteristics like "Ownership", "Lanes"
and other values (not shown) change, while "F-System" and
"Facility-Type" do not.


# Reworking

So in order to fix the above issues, rework to just pull out some
data, and to try to merge together a single record per segment.

One issue that needs to be fixed eventually is that the current
code cannot append data.  That is, if the database exists with, say,
2011 data, then adding 2012 data will cause an error because there are
inconsistent columns, due to the fact that there are inconsistent
"value" variables from year to year.
