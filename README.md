# trmcity

A tiny procedural city that grows on its own schedule. Each run of the
program advances the simulation by one tick — roads creep outward,
buildings spring up beside them, and older blocks densify into taller
ones, OpenTTD-style — then renders an isometric pixel-art view and
regenerates a static site. No server: schedule it a few times a day and
the city quietly grows.

All the art is drawn in code with `racket/draw`. No sprite assets.

## Running

```sh
racket main.rkt                       # advance one tick, regenerate site/
racket main.rkt --local --ticks 50    # fast-forward a local dev city
racket main.rkt --seed 1234           # found a new city with a specific seed
```

The city's state lives in an atproto record on a PDS:
`at://claude.notjack.space/space.notjack.trmcity.map/self`. Each run loads
the record, advances the simulation, and writes it back, so any
machine with the app password (`app_password.txt`, not committed) can
run a tick. The record's tile grid is encoded one character per tile
(`~` water, `.` grass, `t` tree, `#` road, `a`–`l` buildings), so
[viewing the record](https://pdsls.dev/at://claude.notjack.space/space.notjack.trmcity.map/self)
shows an ASCII map of the city.

With `--local` (or when no password file is present) state falls back
to a `state.rktd` file instead. The first run in either mode generates
a 64×64 world (water, grass, forests) and founds the town at its
center. The site is written to `site/`: an `index.html`, `style.css`,
and the rendered `city.png`.

The record schema is defined in
[`lexicons/space.notjack.trmcity.map.json`](lexicons/space.notjack.trmcity.map.json)
and published with `racket publish-lexicon.rkt <handle>` after any
schema change.

## Architecture

- [`main.rkt`](main.rkt) — entry point; load state → tick → save →
  render
- [`private/state.rkt`](private/state.rkt) — the tile grid, tick
  counter, RNG, and both serializations (s-expression file and atproto
  record JSON)
- [`private/atproto.rkt`](private/atproto.rkt) — minimal XRPC client:
  handle resolution, sessions, record get/put
- [`private/terrain.rkt`](private/terrain.rkt) — value-noise world
  generation, runs once
- [`private/growth.rkt`](private/growth.rkt) — the growth rules
- [`private/render.rkt`](private/render.rkt) — isometric PNG renderer
- [`private/site.rkt`](private/site.rkt) — static HTML/CSS output

The simulation is deterministic: the RNG state is saved alongside the
grid, so a city's history is fully reproducible from its founding seed.

## Roadmap

- `_lexicon.trmcity.notjack.space` TXT record pointing at the city account's
  DID, so PDSes can validate the record against the published lexicon
- Deploy the static site to [wisp.place](https://wisp.place) (atproto)
- Move the repo to [Tangled](https://tangled.org) once its CI supports
  scheduled runs; until then GitHub Actions ticks the city three times
  a day (`.github/workflows/tick.yml`, needs a `TRMCITY_APP_PASSWORD`
  Actions secret, and scheduled workflows pause after 60 days without
  repo activity — re-enable from the Actions tab)
- A 800×480 1-bit dithered render for a TRMNL e-ink display
- More tile variety: parks, landmarks, bridges, seasons
