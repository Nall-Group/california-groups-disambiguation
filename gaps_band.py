#!/usr/bin/env python3
"""Slice a gaps_master_* worklist into evenly-sized, content-stable bands.

Standing RA tasks 1243/1244/1245 are split into bands by this helper so several
RAs can work disjoint slices concurrently. A band is "rows i..i+TARGET of the
sorted master" — content-stable: new rows appended by later audit waves slot
into sort order, so a band auto-regrows without changing its number.

Usage:
  python3 gaps_band.py list <category>            # show all bands + pending counts
  python3 gaps_band.py <category> <bandnum>       # print the band's STILL-PENDING rows
  python3 gaps_band.py <category> <bandnum> --all # print all rows (incl. already-handled)

"Pending" = org still present in crosswalk_gaps_all_stances.csv (the source of
truth). Already-handled rows (removed from that CSV) are hidden by default, so a
band shrinks toward empty as it gets worked. When `list` shows 0 pending for a
band, that band's task is Done.

categories: consolidate new_canonical new_chapter individuals partial
            conjoined narrative invalid parens dates not_capitalized
"""
import sys, unicodedata, csv

BIG = 10**9
TARGET = {
    "consolidate": 700, "new_canonical": 360, "new_chapter": BIG,
    "individuals": 1800, "partial": 1400, "conjoined": 700, "narrative": 2000,
    "invalid": 1300, "parens": BIG, "dates": BIG, "not_capitalized": BIG,
}
FILE = {
    "consolidate": "gaps_master_consolidate.tsv",
    "new_canonical": "gaps_master_new_canonicals.txt",
    "new_chapter": "gaps_master_new_chapters.tsv",
    "individuals": "gaps_master_individuals.txt",
    "partial": "gaps_master_partial.txt",
    "conjoined": "gaps_master_conjoined.txt",
    "narrative": "gaps_master_narrative.txt",
    "invalid": "gaps_master_invalid.txt",
    "parens": "gaps_master_parens.txt",
    "dates": "gaps_master_dates.txt",
    "not_capitalized": "gaps_master_not_capitalized.txt",
}

def key(line):
    return unicodedata.normalize("NFKD", line.split("\t")[0].strip()).upper()

def load_rows(cat):
    rows = [l.rstrip("\n") for l in open(FILE[cat], encoding="utf-8") if l.strip()]
    rows.sort(key=key)
    return rows

def bands(cat):
    rows = load_rows(cat)
    t = TARGET[cat]
    return [rows[i:i+t] for i in range(0, len(rows), t)] or [[]]

def pending_orgs():
    s = set()
    with open("crosswalk_gaps_all_stances.csv", encoding="utf-8") as f:
        r = csv.reader(f)
        next(r, None)
        for row in r:
            if row:
                s.add(unicodedata.normalize("NFC", row[0].strip()))
    return s

def is_pending(line, pend):
    org = unicodedata.normalize("NFC", line.split("\t")[0].strip())
    return org in pend

def main():
    a = sys.argv[1:]
    if not a or a[0] in ("-h", "--help"):
        print(__doc__); return
    if a[0] == "list":
        cat = a[1]
        pend = pending_orgs()
        bs = bands(cat)
        print(f"{cat}: {sum(len(b) for b in bs)} logged rows in {len(bs)} band(s)")
        for i, b in enumerate(bs, 1):
            p = sum(1 for l in b if is_pending(l, pend))
            lo = b[0].split('\t')[0][:34] if b else ""
            hi = b[-1].split('\t')[0][:34] if b else ""
            print(f"  band {i:>2}: {p:>5} pending / {len(b):>5} logged   [{lo} .. {hi}]")
        return
    cat, num = a[0], int(a[1])
    show_all = "--all" in a
    bs = bands(cat)
    if not (1 <= num <= len(bs)):
        print(f"band {num} out of range (1..{len(bs)})"); sys.exit(1)
    b = bs[num-1]
    if show_all:
        print("\n".join(b)); return
    pend = pending_orgs()
    keep = [l for l in b if is_pending(l, pend)]
    sys.stderr.write(f"# {cat} band {num}: {len(keep)} pending of {len(b)} logged\n")
    print("\n".join(keep))

if __name__ == "__main__":
    main()
