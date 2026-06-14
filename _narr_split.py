#!/usr/bin/env python3
"""Split gaps_master_narrative.txt into fixed-size batch files for parallel extraction."""
import os

MASTER = "/Users/ruthgracewong/california-groups-disambiguation/gaps_master_narrative.txt"
OUTDIR = "/Users/ruthgracewong/california-groups-disambiguation/_narr_batches"
SIZE = 250


def main():
    os.makedirs(OUTDIR, exist_ok=True)
    with open(MASTER, encoding="utf-8") as f:
        frags = [ln.rstrip("\n") for ln in f if ln.strip() != ""]
    n = len(frags)
    nb = (n + SIZE - 1) // SIZE
    for i in range(nb):
        chunk = frags[i * SIZE:(i + 1) * SIZE]
        with open(os.path.join(OUTDIR, f"batch_{i:04d}.txt"), "w", encoding="utf-8") as f:
            f.write("\n".join(chunk) + "\n")
    print(f"fragments={n} batches={nb} size={SIZE} dir={OUTDIR}")


if __name__ == "__main__":
    main()
