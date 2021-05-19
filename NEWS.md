# norgeo 0.9.1

Introduce functions to download data via API with `get_` prefix:

- `get_list` function to download geo levels via API from SSB.
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
