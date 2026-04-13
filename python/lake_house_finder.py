#!/usr/bin/env python3
# /// script
# requires-python = ">=3.11"
# dependencies = [
#   "geopandas>=0.14",
#   "shapely>=2.0",
#   "pyproj>=3.6",
#   "fiona>=1.9",
#   "pandas>=2.1",
#   "requests>=2.31",
#   "geopy>=2.4",
#   "homeharvest>=0.4",
#   "tabulate>=0.9",
# ]
# ///
"""
lake_house_finder.py
--------------------

Find for-sale homes near lakes in a given geographic area and rank them by
value. Lake proximity is computed against real shapefile data from the US
Census Bureau's TIGER/Line AREAWATER product.

Example:

    uv run python/lake_house_finder.py \\
        --location "Raleigh, NC" \\
        --radius-miles 25 \\
        --max-price 550000 \\
        --max-lake-distance-m 500 \\
        --top 10 \\
        --output /tmp/raleigh.csv
"""

from __future__ import annotations

import argparse
import math
import sys
import time
from pathlib import Path
from typing import TYPE_CHECKING, Iterable

if TYPE_CHECKING:
    import pandas as pd

# Third-party imports (pandas, requests, geopandas, shapely, geopy, homeharvest,
# tabulate) are done lazily inside the functions that need them so that
# `--help` works without the full dep stack installed.

CACHE_DIR = Path(__file__).resolve().parent / "cache"
CACHE_DIR.mkdir(exist_ok=True)

TIGER_YEAR = "2023"
TIGER_BASE = f"https://www2.census.gov/geo/tiger/TIGER{TIGER_YEAR}"
COUNTY_URL = f"{TIGER_BASE}/COUNTY/tl_{TIGER_YEAR}_us_county.zip"
AREAWATER_URL_TMPL = (
    f"{TIGER_BASE}/AREAWATER/tl_{TIGER_YEAR}_{{fips}}_areawater.zip"
)

# MTFCC codes we treat as "lakes" for this tool.
#   H2030 = Lake/Pond
#   H2040 = Reservoir
#   H2050 = Bay/Estuary/Gulf/Sound (inland coves are included; oceans filtered
#           later by --min-lake-area)
LAKE_MTFCCS = {"H2030", "H2040", "H2050"}

# USA Contiguous Albers Equal Area Conic (meters) - good for distance in CONUS.
DISTANCE_CRS = "EPSG:5070"
WGS84 = "EPSG:4326"

USER_AGENT = "lake-house-finder/0.1 (https://github.com/pedroliman/house-hunting)"


# ---------------------------------------------------------------------------
# Geocoding
# ---------------------------------------------------------------------------

def geocode_location(query: str) -> tuple[float, float]:
    """Resolve a free-form location string to (lat, lon) via Nominatim.

    Results are cached on disk under python/cache/geocode.json to stay under
    the Nominatim 1 req/sec policy across repeated runs.
    """
    import json

    cache_file = CACHE_DIR / "geocode.json"
    try:
        cache = json.loads(cache_file.read_text()) if cache_file.exists() else {}
    except Exception:
        cache = {}

    key = query.strip().lower()
    if key in cache:
        lat, lon = cache[key]
        return float(lat), float(lon)

    from geopy.geocoders import Nominatim

    geolocator = Nominatim(user_agent=USER_AGENT)
    loc = geolocator.geocode(query, country_codes="us", timeout=30)
    if loc is None:
        raise RuntimeError(f"Could not geocode location: {query!r}")
    cache[key] = [float(loc.latitude), float(loc.longitude)]
    try:
        cache_file.write_text(json.dumps(cache, indent=2))
    except Exception:
        pass
    return float(loc.latitude), float(loc.longitude)


# ---------------------------------------------------------------------------
# Cached HTTP download
# ---------------------------------------------------------------------------

def download_file(url: str, dest: Path, *, retries: int = 4) -> Path:
    """Download `url` to `dest` with simple exponential backoff. Cached."""
    import requests

    if dest.exists() and dest.stat().st_size > 0:
        return dest

    dest.parent.mkdir(parents=True, exist_ok=True)
    backoff = 2.0
    last_err: Exception | None = None
    for attempt in range(retries):
        try:
            print(f"[download] {url}", file=sys.stderr)
            with requests.get(url, stream=True, timeout=60,
                              headers={"User-Agent": USER_AGENT}) as r:
                r.raise_for_status()
                tmp = dest.with_suffix(dest.suffix + ".part")
                with tmp.open("wb") as f:
                    for chunk in r.iter_content(chunk_size=1 << 15):
                        if chunk:
                            f.write(chunk)
                tmp.rename(dest)
            return dest
        except Exception as e:  # network, HTTP, disk, etc.
            last_err = e
            if attempt == retries - 1:
                break
            time.sleep(backoff)
            backoff *= 2
    raise RuntimeError(f"Failed to download {url}: {last_err}")


# ---------------------------------------------------------------------------
# Counties covering the search buffer
# ---------------------------------------------------------------------------

def counties_for_area(lat: float, lon: float, radius_miles: float) -> list[str]:
    """Return 5-digit county FIPS codes whose geometry intersects the buffer."""
    import geopandas as gpd
    from shapely.geometry import Point

    county_zip = CACHE_DIR / f"tl_{TIGER_YEAR}_us_county.zip"
    download_file(COUNTY_URL, county_zip)

    counties = gpd.read_file(f"zip://{county_zip}")
    # Buffer point in equal-area projection
    pt = gpd.GeoDataFrame(
        {"geometry": [Point(lon, lat)]}, crs=WGS84
    ).to_crs(DISTANCE_CRS)
    buf = pt.buffer(radius_miles * 1609.344).iloc[0]

    counties_proj = counties.to_crs(DISTANCE_CRS)
    hits = counties_proj[counties_proj.intersects(buf)]
    fips = sorted(hits["STATEFP"].astype(str).str.zfill(2)
                  + hits["COUNTYFP"].astype(str).str.zfill(3))
    if not fips:
        raise RuntimeError(
            f"No counties intersect a {radius_miles}-mile buffer around "
            f"({lat:.4f}, {lon:.4f}). Is the location in CONUS?"
        )
    print(f"[counties] {len(fips)} county FIPS: {', '.join(fips)}",
          file=sys.stderr)
    return fips


# ---------------------------------------------------------------------------
# Lake loading
# ---------------------------------------------------------------------------

def load_lakes(fips_list: Iterable[str], min_area_m2: float):
    """Load lake polygons for the given counties as a projected GeoDataFrame."""
    import geopandas as gpd
    import pandas as pd

    frames = []
    for fips in fips_list:
        url = AREAWATER_URL_TMPL.format(fips=fips)
        dest = CACHE_DIR / f"tl_{TIGER_YEAR}_{fips}_areawater.zip"
        try:
            download_file(url, dest)
        except RuntimeError as e:
            # Some counties have no areawater file; skip and continue.
            print(f"[lakes] skipping {fips}: {e}", file=sys.stderr)
            continue
        try:
            gdf = gpd.read_file(f"zip://{dest}")
        except Exception as e:
            print(f"[lakes] could not read {dest.name}: {e}", file=sys.stderr)
            continue
        gdf = gdf[gdf["MTFCC"].isin(LAKE_MTFCCS)]
        if not gdf.empty:
            frames.append(gdf)

    if not frames:
        raise RuntimeError("No lake features found in any intersecting county.")

    lakes = pd.concat(frames, ignore_index=True)
    lakes = gpd.GeoDataFrame(lakes, geometry="geometry", crs=frames[0].crs)
    lakes = lakes.to_crs(DISTANCE_CRS)
    lakes["area_m2"] = lakes.geometry.area
    lakes = lakes[lakes["area_m2"] >= min_area_m2].copy()
    if lakes.empty:
        raise RuntimeError(
            f"All candidate waterbodies were smaller than "
            f"min-lake-area-m2={min_area_m2}. Lower the threshold."
        )
    print(f"[lakes] {len(lakes)} lake/reservoir polygons loaded "
          f"(min area {min_area_m2:.0f} m2)", file=sys.stderr)
    return lakes


# ---------------------------------------------------------------------------
# Listings
# ---------------------------------------------------------------------------

def fetch_listings(
    location: str,
    radius_miles: float,
    max_price: float | None,
    min_price: float | None,
    min_beds: int,
) -> "pd.DataFrame":
    """Fetch for-sale listings via homeharvest (Realtor.com backend)."""
    try:
        from homeharvest import scrape_property
    except ImportError as e:
        raise RuntimeError(
            "homeharvest is required. Run via `uv run python/lake_house_finder.py` "
            "(the PEP 723 header pulls all deps automatically) or `uv sync` "
            "inside ./python."
        ) from e

    # Current homeharvest (>=0.4) scrapes Realtor.com only. The older
    # `site_name` kwarg for redfin/zillow is gone. Realtor.com listings cover
    # both Redfin and Zillow inventories in practice (same MLS feeds).
    try:
        df = scrape_property(
            location=location,
            listing_type="for_sale",
            radius=radius_miles,
            price_max=int(max_price) if max_price is not None else None,
            price_min=int(min_price) if min_price is not None else None,
            beds_min=min_beds if min_beds > 0 else None,
            exclude_pending=True,
        )
    except Exception as e:
        raise RuntimeError(
            f"Listing scrape failed: {e}\n"
            f"Workaround: export a CSV manually from Redfin/Zillow/Realtor "
            f"and pass it with --listings-csv."
        ) from e

    if df is None or len(df) == 0:
        raise RuntimeError("No listings returned.")

    # Normalize column names. homeharvest returns 'list_price' already in
    # recent versions but older schemas used 'price'.
    rename = {}
    if "list_price" not in df.columns and "price" in df.columns:
        rename["price"] = "list_price"
    df = df.rename(columns=rename)

    needed = {"latitude", "longitude", "list_price"}
    missing = needed - set(df.columns)
    if missing:
        raise RuntimeError(
            f"Listings frame is missing expected columns: {missing}. "
            f"Got: {sorted(df.columns)[:20]}..."
        )

    df = df.dropna(subset=["latitude", "longitude", "list_price"]).copy()
    if max_price is not None:
        df = df[df["list_price"] <= max_price]
    if min_price is not None:
        df = df[df["list_price"] >= min_price]
    return df.reset_index(drop=True)


def load_listings_csv(path: Path) -> "pd.DataFrame":
    """Load listings from a user-supplied CSV (manual export fallback)."""
    import pandas as pd

    df = pd.read_csv(path)
    # Accept common Redfin CSV headers, normalize to what the rest of the tool
    # expects (latitude, longitude, list_price).
    colmap = {
        "LATITUDE": "latitude",
        "LONGITUDE": "longitude",
        "PRICE": "list_price",
        "Latitude": "latitude",
        "Longitude": "longitude",
        "Price": "list_price",
        "SQUARE FEET": "sqft",
        "BEDS": "beds",
        "BATHS": "baths",
        "URL (SEE https://www.redfin.com/buy-a-home/comparative-market-analysis FOR INFO ON PRICING)": "property_url",
        "URL": "property_url",
        "ADDRESS": "address",
    }
    for src, dst in colmap.items():
        if src in df.columns and dst not in df.columns:
            df = df.rename(columns={src: dst})
    for col in ("latitude", "longitude", "list_price"):
        if col not in df.columns:
            raise RuntimeError(
                f"--listings-csv missing column {col!r}; got: {sorted(df.columns)}"
            )
    return df.dropna(subset=["latitude", "longitude", "list_price"]).reset_index(drop=True)


# ---------------------------------------------------------------------------
# Distance and scoring
# ---------------------------------------------------------------------------

def distance_to_nearest_lake(listings_df: "pd.DataFrame", lakes_gdf) -> "pd.DataFrame":
    """Attach lake_dist_m column (meters to nearest lake polygon edge)."""
    import geopandas as gpd
    from shapely.geometry import Point

    pts = gpd.GeoDataFrame(
        listings_df.copy(),
        geometry=[Point(xy) for xy in zip(listings_df["longitude"],
                                           listings_df["latitude"])],
        crs=WGS84,
    ).to_crs(DISTANCE_CRS)

    nearest = gpd.sjoin_nearest(
        pts, lakes_gdf[["geometry"]], how="left", distance_col="lake_dist_m"
    )
    # sjoin_nearest can duplicate rows when ties; collapse by min distance.
    nearest = (
        nearest.sort_values("lake_dist_m")
        .drop_duplicates(subset=pts.index.name or "level_0", keep="first")
        if False  # the reset-index trick below is simpler
        else nearest
    )
    nearest = nearest.loc[~nearest.index.duplicated(keep="first")]
    listings_df = listings_df.copy()
    listings_df["lake_dist_m"] = nearest["lake_dist_m"].values
    return listings_df


POSITIVE_WATERFRONT_KEYWORDS = [
    "lakefront", "lake front", "lake-front",
    "waterfront", "water front", "water-front",
    "on the lake", "on the water",
    "lake frontage", "water frontage",
    "private dock", "boat dock", "boat slip", "boat house", "boathouse",
    "pier", "water access", "lake access",
    "lake view", "views of the lake",
    "at the lake",
]

# If a description contains ONLY pond-y words (and none of the strong
# waterfront phrases), we suppress the lakefront signal.
POND_ONLY_KEYWORDS = ["pond", "community pond", "neighborhood pond"]


def _lakefront_confidence(row) -> float:
    """Heuristic 0..1 score that a listing is *actually* on a lake.

    Combines:
      - How close the listed point is to the nearest lake polygon.
      - Whether the estimated lot radius (from lot_sqft) can plausibly
        reach the water from the listed point.
      - Waterfront keywords in the listing description/text.
    """
    import math as _m

    dist = float(row.get("lake_dist_m") or 1e9)
    lot_sqft = row.get("lot_sqft")
    text = (row.get("text") or "")
    if not isinstance(text, str):
        text = ""
    text_lc = text.lower()

    # 1) Geometric signal: does the parcel plausibly touch water?
    # Approximate the lot as a circle with area = lot_sqft (in m^2); compare
    # its radius to the house-point-to-lake distance. If the radius >= the
    # distance, the parcel boundary likely intersects the water.
    try:
        lot_m2 = float(lot_sqft) * 0.09290304 if lot_sqft else 0.0
    except (TypeError, ValueError):
        lot_m2 = 0.0
    lot_radius_m = _m.sqrt(lot_m2 / _m.pi) if lot_m2 > 0 else 0.0
    edge_dist_m = max(0.0, dist - lot_radius_m)

    # Geometric score (closer edge -> higher).
    if edge_dist_m <= 0:
        geom = 1.0
    elif edge_dist_m <= 15:
        geom = 0.85
    elif edge_dist_m <= 40:
        geom = 0.6
    elif edge_dist_m <= 80:
        geom = 0.3
    else:
        geom = 0.0

    # 2) Text signal.
    pos_hits = sum(1 for kw in POSITIVE_WATERFRONT_KEYWORDS if kw in text_lc)
    has_pos = pos_hits > 0
    has_pond = any(kw in text_lc for kw in POND_ONLY_KEYWORDS) and not has_pos

    text_score = 0.0
    if has_pos:
        # Diminishing returns past a couple of hits.
        text_score = min(1.0, 0.5 + 0.25 * pos_hits)
    if has_pond:
        text_score = -0.3  # penalty: likely community pond, not a lake

    # 3) Hard overrides: very close to water alone deserves high confidence.
    if dist <= 15:
        point_bonus = 0.4
    elif dist <= 40:
        point_bonus = 0.2
    else:
        point_bonus = 0.0

    conf = 0.55 * geom + 0.35 * text_score + point_bonus
    return max(0.0, min(1.0, conf))


def add_lakefront_confidence(df: "pd.DataFrame") -> "pd.DataFrame":
    """Attach lakefront_conf + supporting columns."""
    import math as _m

    out = df.copy()
    # Populate supporting columns for CSV transparency.
    lot_m2 = out.get("lot_sqft")
    if lot_m2 is not None:
        radii = []
        for v in lot_m2:
            try:
                x = float(v) * 0.09290304
                radii.append(_m.sqrt(x / _m.pi) if x > 0 else 0.0)
            except (TypeError, ValueError):
                radii.append(0.0)
        out["lot_radius_m"] = radii
        out["edge_dist_m"] = (
            out["lake_dist_m"].astype(float) - out["lot_radius_m"]
        ).clip(lower=0.0)
    else:
        out["lot_radius_m"] = 0.0
        out["edge_dist_m"] = out["lake_dist_m"].astype(float)

    out["lakefront_conf"] = out.apply(_lakefront_confidence, axis=1)
    return out


def score_value(
    df: "pd.DataFrame", weight_price: float, weight_lake: float
) -> "pd.DataFrame":
    """Compute a 0..1 composite value score; higher is better."""
    import pandas as pd

    out = df.copy()

    if "sqft" in out.columns:
        sqft = pd.to_numeric(out["sqft"], errors="coerce")
    else:
        sqft = pd.Series([math.nan] * len(out), index=out.index)
    # Fall back to raw price where sqft missing.
    ppsf = out["list_price"] / sqft
    ppsf = ppsf.where(sqft.notna() & (sqft > 0), out["list_price"] / 1000.0)
    out["price_per_sqft"] = ppsf

    def minmax_norm(s: pd.Series) -> pd.Series:
        lo, hi = s.min(), s.max()
        if not math.isfinite(lo) or not math.isfinite(hi) or hi == lo:
            return pd.Series([0.0] * len(s), index=s.index)
        return (s - lo) / (hi - lo)

    ppsf_norm = minmax_norm(ppsf)
    dist_norm = minmax_norm(out["lake_dist_m"])
    out["value_score"] = (
        weight_price * (1.0 - ppsf_norm) + weight_lake * (1.0 - dist_norm)
    )
    return out


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    p = argparse.ArgumentParser(
        description="Find for-sale houses near lakes and rank them by value.",
    )
    p.add_argument("--location", required=True,
                   help='e.g. "Raleigh, NC" or "27601".')
    p.add_argument("--radius-miles", type=float, default=25.0,
                   help="Search radius around the geocoded location.")
    p.add_argument("--min-price", type=float, default=None,
                   help="Minimum list price in USD (e.g. 400000).")
    p.add_argument("--max-price", type=float, default=None,
                   help="Maximum list price in USD (e.g. 550000).")
    p.add_argument("--min-beds", type=int, default=0,
                   help="Minimum number of bedrooms.")
    p.add_argument("--max-lake-distance-m", type=float, default=500.0,
                   help="Keep only listings within this many meters of a lake.")
    p.add_argument("--min-lakefront-conf", type=float, default=0.0,
                   help="Drop listings whose lakefront_conf is below this "
                        "(0..1). 0.7 is a good 'truly lakefront' filter.")
    p.add_argument("--lakefront-only", action="store_true",
                   help="Shortcut for --min-lakefront-conf 0.7.")
    p.add_argument("--min-lake-area-m2", type=float, default=10_000.0,
                   help="Drop waterbodies smaller than this (filters tiny ponds "
                        "and also ocean polygons if set large).")
    p.add_argument("--listings-csv", type=Path, default=None,
                   help="Bypass scraping; read listings from this CSV "
                        "(must have latitude, longitude, list_price columns).")
    p.add_argument("--weight-price", type=float, default=0.6,
                   help="Value-score weight on price per sqft (0..1).")
    p.add_argument("--weight-lake", type=float, default=0.4,
                   help="Value-score weight on lake proximity (0..1).")
    p.add_argument("--top", type=int, default=20,
                   help="How many top results to print.")
    p.add_argument("--output", type=Path, default=None,
                   help="Optional path to write the full filtered+scored CSV.")
    args = p.parse_args(argv)

    if abs((args.weight_price + args.weight_lake) - 1.0) > 1e-6:
        p.error("--weight-price and --weight-lake must sum to 1.0")
    return args


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)

    import pandas as pd
    from tabulate import tabulate


    print(f"[geocode] resolving {args.location!r} ...", file=sys.stderr)
    lat, lon = geocode_location(args.location)
    print(f"[geocode] {args.location!r} -> ({lat:.5f}, {lon:.5f})",
          file=sys.stderr)

    fips = counties_for_area(lat, lon, args.radius_miles)
    lakes = load_lakes(fips, args.min_lake_area_m2)

    if args.listings_csv is not None:
        print(f"[listings] loading {args.listings_csv}", file=sys.stderr)
        listings = load_listings_csv(args.listings_csv)
        if args.max_price is not None:
            listings = listings[listings["list_price"] <= args.max_price]
        if args.min_price is not None:
            listings = listings[listings["list_price"] >= args.min_price]
        if args.min_beds > 0 and "beds" in listings.columns:
            listings = listings[
                pd.to_numeric(listings["beds"], errors="coerce") >= args.min_beds
            ]
    else:
        print(f"[listings] scraping Realtor.com around {args.location!r} "
              f"(radius {args.radius_miles} mi)", file=sys.stderr)
        listings = fetch_listings(
            args.location, args.radius_miles,
            args.max_price, args.min_price, args.min_beds,
        )
    print(f"[listings] {len(listings)} candidates after price/beds filter",
          file=sys.stderr)

    if len(listings) == 0:
        print("No listings found matching price filter.", file=sys.stderr)
        return 2

    listings = distance_to_nearest_lake(listings, lakes)
    near = listings[listings["lake_dist_m"] <= args.max_lake_distance_m].copy()
    print(f"[filter] {len(near)} listings within {args.max_lake_distance_m:.0f} m of a lake",
          file=sys.stderr)

    if len(near) == 0:
        print("No listings within the lake-distance threshold.", file=sys.stderr)
        return 2

    near = add_lakefront_confidence(near)
    min_conf = 0.7 if args.lakefront_only else args.min_lakefront_conf
    if min_conf > 0:
        before = len(near)
        near = near[near["lakefront_conf"] >= min_conf].copy()
        print(f"[filter] {len(near)}/{before} listings pass "
              f"lakefront_conf >= {min_conf:.2f}", file=sys.stderr)

    if len(near) == 0:
        print("No listings meet the lakefront-confidence threshold.",
              file=sys.stderr)
        return 2

    scored = score_value(near, args.weight_price, args.weight_lake)
    scored = scored.sort_values(
        ["lakefront_conf", "value_score"], ascending=[False, False]
    )

    # Pick display columns that are commonly present.
    display_cols = [c for c in (
        "formatted_address", "address", "list_price", "beds", "sqft",
        "price_per_sqft", "lake_dist_m", "edge_dist_m", "lakefront_conf",
        "value_score", "property_url",
    ) if c in scored.columns]

    top = scored.head(args.top)
    print(tabulate(
        top[display_cols].round({"price_per_sqft": 0,
                                 "lake_dist_m": 0,
                                 "edge_dist_m": 0,
                                 "lakefront_conf": 2,
                                 "value_score": 3}),
        headers="keys", tablefmt="github", showindex=False,
    ))

    if args.output is not None:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        scored.to_csv(args.output, index=False)
        print(f"\n[output] wrote {len(scored)} rows to {args.output}",
              file=sys.stderr)

    return 0


if __name__ == "__main__":
    sys.exit(main())
