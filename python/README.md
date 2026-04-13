# Lake House Finder

A Python CLI tool that searches **Redfin** or **Zillow** for for-sale homes
near lakes inside a geographic area and ranks them by value.

Lake proximity is computed against real polygon data from the US Census
Bureau's **TIGER/Line AREAWATER** shapefiles — not just scraped listing text.

This lives alongside the R/Shiny analysis in the parent repo; it does not
depend on or modify that code.

---

## Install / Run

Dependencies are managed with [**uv**](https://docs.astral.sh/uv/).

### Option A — one-shot, zero setup (PEP 723)

`lake_house_finder.py` has an inline PEP 723 dependency header, so from the
repo root you can just:

```bash
uv run python/lake_house_finder.py --help
```

uv will resolve and cache the deps automatically on first run.

### Option B — project mode

```bash
cd python
uv sync
uv run lake_house_finder.py --help
```

---

## Example: the request from the task description

> "If I ask for houses near Raleigh next to a lake, are you able to find
> houses for sale for under 550k?"

```bash
uv run python/lake_house_finder.py \
    --location "Raleigh, NC" \
    --radius-miles 25 \
    --max-price 550000 \
    --max-lake-distance-m 500 \
    --min-beds 3 \
    --top 10 \
    --output /tmp/raleigh_lake_houses.csv
```

Output is a ranked GitHub-flavored Markdown table on stdout plus a full CSV
at `--output`. Progress goes to stderr so stdout stays pipe-friendly.

---

## How it works

1. **Geocode** the `--location` string with OSM Nominatim.
2. Compute the set of **counties** whose polygons intersect the search buffer
   (using the TIGER `tl_2023_us_county.zip` layer).
3. For each county, download its TIGER **AREAWATER** shapefile and keep
   waterbody polygons whose MTFCC ∈ {`H2030` lake/pond, `H2040` reservoir,
   `H2050` bay/estuary/cove}. Waterbodies smaller than
   `--min-lake-area-m2` (default 10,000 m²) are discarded.
4. **Scrape listings** with the `homeharvest` library against the chosen
   site (`redfin` | `zillow` | `realtor.com`).
5. **Distance**: project both layers to EPSG:5070 (CONUS Albers Equal Area)
   and use `geopandas.sjoin_nearest` to attach a `lake_dist_m` column to
   each listing.
6. **Filter & rank**: drop listings above `--max-price`, below `--min-beds`,
   or farther than `--max-lake-distance-m` from any lake. Score the rest:

   ```
   value_score = w_price * (1 − ppsf_norm) + w_lake * (1 − lake_dist_norm)
   ```

   where `ppsf = list_price / sqft`, each component is min-max normalized
   over the filtered set, and weights default to `0.6 / 0.4`.

Downloaded shapefiles are cached under `python/cache/` so subsequent runs
are fast.

---

## Flags

| Flag | Default | Purpose |
|------|---------|---------|
| `--location` | (required) | City, address, or ZIP to center the search on. |
| `--radius-miles` | `25` | Search radius around the geocoded center. |
| `--max-price` | `None` | Hard cap on list price. |
| `--min-beds` | `0` | Drop listings with fewer bedrooms. |
| `--max-lake-distance-m` | `500` | Proximity threshold to the nearest lake edge. |
| `--min-lake-area-m2` | `10000` | Drop tiny ponds / (if raised) ocean polygons. |
| `--source` | `redfin` | One of `redfin`, `zillow`, `realtor.com`. |
| `--listings-csv` | `None` | Bypass scraping — read pre-exported listings. |
| `--weight-price` | `0.6` | Weight on price/sqft in value score. |
| `--weight-lake`  | `0.4` | Weight on lake proximity in value score. |
| `--top` | `20` | Print the top N rows. |
| `--output` | `None` | Write full filtered+scored CSV to this path. |

`--weight-price + --weight-lake` must equal `1.0`.

---

## Caveats (please read)

- **Scraping is fragile.** Neither Redfin nor Zillow publish a free public
  listing API. `homeharvest` is a best-effort scraper; if the site changes
  its frontend the scrape will break. When it does, export a CSV manually
  from the site's map search and re-run with
  `--listings-csv my_export.csv`.
- **TIGER AREAWATER is coarser than USGS NHD.** Very small ponds may be
  missing. Conversely, very large polygons (Pamlico Sound, the Atlantic
  near coastal NC) share MTFCC `H2050` and would otherwise swamp results;
  in practice the `--max-lake-distance-m 500` filter plus the
  `--min-lake-area-m2` knob handle this well for inland searches. For a
  strictly-freshwater search, set `--min-lake-area-m2 10_000_000` to
  require ~10 km² (gets the major named lakes only) and then widen again.
- **Nominatim geocoding** is rate-limited to 1 req/sec by the OSM ToS. Fine
  for one-shot usage.
- **CONUS only.** Distances are computed in EPSG:5070 (CONUS Albers).
  Hawaii/Alaska/territories are not supported without a CRS change.
- **First run is slow.** The nationwide county file is ~80 MB, plus one
  areawater zip per intersecting county (typically <5 MB each). Everything
  is cached under `python/cache/` thereafter.
