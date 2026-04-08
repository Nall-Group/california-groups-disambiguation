"""
Build chunked lookup files from the JSON crosswalk for the static GitHub Pages app.

Reads 2_webapp/org_clusters_crosswalk.json and generates docs/chunks/ with:
- 2-char prefix files for small buckets (<200KB)
- 3-char prefix sub-files for medium buckets
- Hash-split sub-files for large 3-char buckets (e.g. CAL, THE, SAN)

Run: python3 scripts/build_chunks.py
"""

import json
import math
import os
import shutil
import sys
from collections import defaultdict

SIZE_THRESHOLD = 200_000  # bytes


def fnv1a_32(s):
    """FNV-1a 32-bit hash. Must match the JS implementation exactly."""
    h = 0x811C9DC5
    for b in s.encode("utf-8"):
        h = ((h ^ b) * 0x01000193) & 0xFFFFFFFF
    return h


def flatten_crosswalk(path):
    """Read JSON crosswalk and return flat dict: name -> {c, r}."""
    with open(path) as f:
        data = json.load(f)

    entries = {}

    def walk(children, canonical):
        for child in children:
            name = child.get("name", "")
            rel = child.get("relationship", "")
            if name:
                entries[name] = {"c": canonical, "r": rel}
            if child.get("children"):
                walk(child["children"], canonical)

    for cluster in data["clusters"]:
        canonical = cluster.get("canonical", "")
        if not canonical:
            continue
        entries[canonical] = {"c": canonical, "r": "self"}
        if cluster.get("children"):
            walk(cluster["children"], canonical)

    return entries


def estimate_size(items):
    """Estimate JSON size of a chunk dict."""
    return sum(len(n) + len(v["c"]) + len(v["r"]) + 20 for n, v in items)


def write_chunk(path, data):
    """Write a chunk JSON file."""
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w") as f:
        json.dump(data, f, separators=(",", ":"))
    return os.path.getsize(path)


def build_chunks(entries, out_dir):
    """Group entries into chunk files using hybrid prefix/hash splitting."""
    # Group by 2-char prefix
    by_p2 = defaultdict(dict)
    for name, val in entries.items():
        p = name.upper().strip()
        p2 = p[:2] if len(p) >= 2 else p
        by_p2[p2][name] = val

    manifest = {"split": {}}
    stats = {"files": 0, "max_size": 0, "total_size": 0}

    for p2, items in sorted(by_p2.items()):
        size = estimate_size(items.items())

        if size <= SIZE_THRESHOLD:
            # Write single 2-char file
            path = os.path.join(out_dir, f"{p2}.json")
            fsize = write_chunk(path, items)
            stats["files"] += 1
            stats["max_size"] = max(stats["max_size"], fsize)
            stats["total_size"] += fsize
            continue

        # Split into 3-char sub-groups
        manifest["split"][p2] = {}
        by_p3 = defaultdict(dict)
        for name, val in items.items():
            p = name.upper().strip()
            p3 = p[:3] if len(p) >= 3 else p
            by_p3[p3][name] = val

        for p3, sub_items in sorted(by_p3.items()):
            sub_size = estimate_size(sub_items.items())

            if sub_size <= SIZE_THRESHOLD:
                # Write single 3-char file
                path = os.path.join(out_dir, p2, f"{p3}.json")
                fsize = write_chunk(path, sub_items)
                manifest["split"][p2][p3] = 0
                stats["files"] += 1
                stats["max_size"] = max(stats["max_size"], fsize)
                stats["total_size"] += fsize
            else:
                # Hash-split
                n_buckets = math.ceil(sub_size / SIZE_THRESHOLD)
                manifest["split"][p2][p3] = n_buckets
                buckets = defaultdict(dict)
                for name, val in sub_items.items():
                    h = fnv1a_32(name.upper()) % n_buckets
                    buckets[h][name] = val

                for h, bucket_items in sorted(buckets.items()):
                    path = os.path.join(out_dir, p2, f"{p3}_{h}.json")
                    fsize = write_chunk(path, bucket_items)
                    stats["files"] += 1
                    stats["max_size"] = max(stats["max_size"], fsize)
                    stats["total_size"] += fsize

    # Write manifest
    manifest_path = os.path.join(out_dir, "manifest.json")
    with open(manifest_path, "w") as f:
        json.dump(manifest, f, separators=(",", ":"))
    stats["files"] += 1

    return manifest, stats


def main():
    src = os.path.join("2_webapp", "org_clusters_crosswalk.json")
    out_dir = os.path.join("docs", "chunks")

    if not os.path.exists(src):
        print(f"Error: {src} not found. Run from project root.", file=sys.stderr)
        sys.exit(1)

    # Clean output directory
    if os.path.exists(out_dir):
        shutil.rmtree(out_dir)
    os.makedirs(out_dir)

    print(f"Reading {src}...")
    entries = flatten_crosswalk(src)
    print(f"  {len(entries):,} unique names")

    print(f"Building chunks in {out_dir}/...")
    manifest, stats = build_chunks(entries, out_dir)

    n_split_p2 = len(manifest["split"])
    n_hash_split = sum(1 for p2 in manifest["split"] for p3, n in manifest["split"][p2].items() if n > 0)

    print(f"\nDone!")
    print(f"  Chunk files: {stats['files']:,}")
    print(f"  Total size:  {stats['total_size'] / 1024 / 1024:.1f} MB")
    print(f"  Max chunk:   {stats['max_size'] / 1024:.0f} KB")
    print(f"  2-char prefixes split into 3-char: {n_split_p2}")
    print(f"  3-char prefixes hash-split: {n_hash_split}")


if __name__ == "__main__":
    main()
