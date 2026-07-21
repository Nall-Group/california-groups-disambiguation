# Crosswalk lookup — static single-file web app

A fully client-side disambiguation tool: one small page + one prebuilt index file,
served as static assets (e.g. GitHub Pages). **No R and no server** — every lookup,
single or batch, runs in the browser in memory.

This is the Python/JS successor to two older pieces:

- **`2_webapp/app.R`** — the R **Shiny** app. Needs a live R server, reloads a 23 MB
  JSON at every cold start, and does a network round-trip per lookup. Kept only as a
  reference; not needed to run this.
- **`docs/` + `scripts/build_chunks.py`** — the previous static app, which sharded the
  data into ~1,300 prefix files. Two problems this version fixes:
  1. **Windows-hostile filenames.** Chunks were named by org-name prefix, producing
     paths like `docs/chunks/C?.json`, `E*.json`, `K".json`, `RE:.json`. `?`, `*`, `"`,
     `:` are illegal on Windows/NTFS, so `git checkout` **fails** on any Windows machine.
  2. **Slow batch.** A big CSV had to fetch many separate chunk files.

## How it works

`scripts/build_index.py` flattens `2_webapp/org_clusters_crosswalk.json` into one file,
`web/crosswalk_index.json` (~13 MB raw, **~3 MB gzipped** — GitHub Pages gzips it on the
wire). The browser downloads it once, then:

- **Single lookup:** exact → case-insensitive exact → global token-subset fuzzy.
  (The old app's fuzzy could only see names sharing a prefix; here it sees all
  212k names, so it finds strictly more candidates.)
- **Batch CSV:** ~5,000 lookups in a few milliseconds, fully offline.

### Index format

```json
{
  "rels":       ["self", "alternate_spelling", "chapter", ...],
  "canonicals": ["American Civil Liberties Union", ...],
  "names":      { "ACLU": [<canonicalId>, <relId>], ... }
}
```
A name whose `relId` is `"self"` is itself a canonical.

## Rebuild after crosswalk changes

Whenever `2_webapp/org_clusters_crosswalk.json` changes:

```bash
python3 scripts/build_index.py     # run from the project root -> web/crosswalk_index.json
```

## Run locally

`fetch()` needs HTTP (not `file://`), so serve the folder:

```bash
cd web
python3 -m http.server 8000
# open http://127.0.0.1:8000/
```

## Deploy

GitHub Pages serves static files with gzip automatically. To make this the public app,
publish `web/index.html` + `web/crosswalk_index.json` at the Pages root (e.g. move them
into `docs/`, replacing the old `docs/index.html` + `docs/chunks/`). Left as a separate
`web/` folder for now so it can be reviewed side-by-side with the existing `docs/` app.
