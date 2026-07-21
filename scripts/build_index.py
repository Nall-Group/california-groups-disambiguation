"""
Build a single compact lookup index for the static crosswalk web app.

Reads 2_webapp/org_clusters_crosswalk.json and writes web/crosswalk_index.json:
one file the browser downloads once, then serves 200k+ lookups (single AND batch)
entirely in memory -- no server, no per-query network fetches, no R.

This replaces scripts/build_chunks.py (which sharded the data into ~1,300 prefix
files -- an approach that (a) produced filesystem-illegal names on Windows like
`C?.json`, `E*.json`, `RE:.json`, breaking `git checkout`, and (b) needed many
fetches for a batch CSV). GitHub Pages gzip-compresses the JSON on the wire
(~13 MB raw -> ~3 MB transferred).

Index format (all lookups reference these by integer id to avoid repeating strings):
  {
    "rels":       [relationship strings; index 0 is always "self"],
    "canonicals": [canonical org strings],
    "names":      { "<any known name>": [canonicalId, relId] }
  }
A name whose relId maps to "self" IS a canonical (canonicals[canonicalId] == name).

Run from the project root:  python3 scripts/build_index.py
"""

import json
import os
import sys

SRC = os.path.join("2_webapp", "org_clusters_crosswalk.json")
OUT = os.path.join("web", "crosswalk_index.json")


def build_index(data):
    rel_codes = {"self": 0}          # relationship string -> id (0 reserved for "self")
    canon_index = {}                 # canonical string -> id
    canonicals = []                  # id -> canonical string
    names = {}                       # name -> [canonicalId, relId]

    def rel_id(r):
        i = rel_codes.get(r)
        if i is None:
            i = len(rel_codes)
            rel_codes[r] = i
        return i

    def canon_id(c):
        i = canon_index.get(c)
        if i is None:
            i = len(canonicals)
            canon_index[c] = i
            canonicals.append(c)
        return i

    def walk(children, cid):
        for child in children:
            name = child.get("name", "")
            if name:
                # last-write-wins on duplicate names, matching build_chunks.py
                names[name] = [cid, rel_id(child.get("relationship", ""))]
            if child.get("children"):
                walk(child["children"], cid)

    for cluster in data["clusters"]:
        canonical = cluster.get("canonical", "")
        if not canonical:
            continue
        cid = canon_id(canonical)
        names[canonical] = [cid, 0]          # the canonical maps to itself, rel "self"
        if cluster.get("children"):
            walk(cluster["children"], cid)

    rels = [r for r, _ in sorted(rel_codes.items(), key=lambda kv: kv[1])]
    return {"rels": rels, "canonicals": canonicals, "names": names}


def main():
    if not os.path.exists(SRC):
        print(f"Error: {SRC} not found. Run from the project root.", file=sys.stderr)
        sys.exit(1)

    print(f"Reading {SRC} ...")
    with open(SRC, encoding="utf-8") as f:
        data = json.load(f)

    index = build_index(data)

    os.makedirs(os.path.dirname(OUT), exist_ok=True)
    with open(OUT, "w", encoding="utf-8") as f:
        json.dump(index, f, separators=(",", ":"), ensure_ascii=False)

    size_mb = os.path.getsize(OUT) / 1024 / 1024
    print("Done.")
    print(f"  searchable names:  {len(index['names']):,}")
    print(f"  unique canonicals: {len(index['canonicals']):,}")
    print(f"  relationships:     {index['rels']}")
    print(f"  {OUT}: {size_mb:.1f} MB raw (GitHub Pages serves it gzipped, ~3 MB)")


if __name__ == "__main__":
    main()
