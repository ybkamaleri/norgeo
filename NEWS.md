# norgeo 0.9.3

- All geo codes downloaded via API can be cast for geo granularity with `cast_geo`

  | codes   | year | level   | grks    | fylke | kommune | bydel  |
  |---------|------|---------|---------|-------|---------|--------|
  | 0320333 | 2021 | grks    | 0333333 | 03    | 0320    | 032141 |
  | 0322    | 2021 | kommune | NA      | 03    | 0322    | NA     |

- Gives error message if specification in `get_correspond()` for `type` and
  `correspond` in oppsite order.
- Give error message if year specification in `from` and `to` in a wrong order.
- Stop if there is no code change for the specified year when running `track_change()`

# norgeo 0.9.2

All API functions now use arguments `from` and `to` instead of `year` as in `geo_` functions.
This is matching the specification from SSB API Klass.

New features are introduced to help working with the downloaded data from API. Now you can use:

- `track_change` to get all code changes until the date specified year in `to` argument
- `track_split` to find geo codes that are split to different geo codes
- `track_merge` to find geo codes that are merged to a new geo

# norgeo 0.9.1


Introduce functions to download data via API with `get_` prefix:

- `get_code` function to download geo levels via API from SSB.
- `get_change` function to download code changes.
- `get_correspond` function to get geo codes for corresponding granularity.
- `get_set` function is now a wrapper function for `get_list` and `get_change`

# norgeo 0.9.0

* The real first release version

# norgeo 1.3.3

Despite it was version 1.3.3 til then, unfortunately `norgeo` was actually
premature since it hasn't been tested properly, but was released due to
the need to use the function. So that was much too early and I have to
pull it back and make rebirth of `norgeo 0.9` with a bit properly thought
function names and structure. But of course there are always errors and
things that can be better... Anyway, `norgeo 1.3.3` before
`norgeo 0.9.0` works but too ambitious to call it 1.3.3, but hopefully
the second future version of `norgeo 1.3.3` will be much better :-)

# norgeo (development version)

## `dev` branch

All ongoing new ideas will be implemented here. So contibutions
and bugs reports are very much welcome.

## Track changes

* `geo_merge()` function provide output for `split` and `merge` codes,
but it's only relevant to those that are splitted or merged in the most
recent code list. All that happened prior to this haven't been handled
properly. So may be you can help to solve this?

## Change table

Hopefully a better alternative to solve this challenge when SSB
doesn't provide codes that have changed. May be SSB sees the problem
and will fix it to help external users.
