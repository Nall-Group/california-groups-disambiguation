#!/usr/bin/env python3
"""List all adjacent canonical pairs, grouped by first letter, with similarity scores."""

import json
import re
import sys
from collections import defaultdict

CROSSWALK_PATH = "2_webapp/org_clusters_crosswalk.json"


def normalize(name):
    """Normalize for comparison: lowercase, strip punctuation, collapse whitespace."""
    name = name.lower()
    name = name.replace("&", "and")
    name = re.sub(r"[^\w\s]", " ", name)
    return re.sub(r"\s+", " ", name).strip()


def shared_word_count(a, b):
    """Count shared significant words between two names."""
    stopwords = {"of", "the", "and", "a", "in", "for", "to", "on", "at", "by", "an", "or", "inc", "llc"}
    words_a = set(w for w in normalize(a).split() if w not in stopwords and len(w) > 1)
    words_b = set(w for w in normalize(b).split() if w not in stopwords and len(w) > 1)
    if not words_a or not words_b:
        return 0, 0.0
    shared = len(words_a & words_b)
    jaccard = shared / len(words_a | words_b)
    return shared, jaccard


def is_prefix(a, b):
    """Check if a is a prefix of b (normalized)."""
    na, nb = normalize(a), normalize(b)
    return nb.startswith(na) or na.startswith(nb)


def main():
    with open(CROSSWALK_PATH, encoding="utf-8") as f:
        data = json.load(f)

    canonicals = sorted([e["canonical"] for e in data["clusters"]], key=str.lower)
    children_count = {}
    for e in data["clusters"]:
        children_count[e["canonical"]] = len(e.get("children", []))

    print(f"Loaded {len(canonicals)} canonicals", file=sys.stderr)

    # Group by first character
    groups = defaultdict(list)
    for i in range(len(canonicals) - 1):
        a, b = canonicals[i], canonicals[i + 1]
        first_char = a[0].upper() if a[0].isalpha() else "#"
        shared, jaccard = shared_word_count(a, b)
        prefix = is_prefix(a, b)
        groups[first_char].append((a, b, shared, jaccard, prefix))

    # Output
    output_file = sys.argv[1] if len(sys.argv) > 1 else "adjacent_pairs.txt"
    with open(output_file, "w") as f:
        total = sum(len(v) for v in groups.values())
        f.write(f"Adjacent canonical pairs: {total}\n")
        f.write(f"{'=' * 80}\n\n")

        for letter in sorted(groups.keys()):
            pairs = groups[letter]
            f.write(f"\n### {letter} ({len(pairs)} pairs) ###\n\n")
            # Sort by jaccard desc so highest similarity first
            for a, b, shared, jaccard, prefix in sorted(pairs, key=lambda x: -x[3]):
                ac = children_count.get(a, 0)
                bc = children_count.get(b, 0)
                flags = []
                if prefix:
                    flags.append("PREFIX")
                if jaccard >= 0.8:
                    flags.append("HIGH")
                elif jaccard >= 0.5:
                    flags.append("MED")
                flag_str = f" [{','.join(flags)}]" if flags else ""
                f.write(f"J={jaccard:.2f} S={shared}{flag_str}\n")
                f.write(f"  [{ac}c] {a}\n")
                f.write(f"  [{bc}c] {b}\n\n")

    print(f"Output written to {output_file}", file=sys.stderr)
    print(f"Total pairs: {total}", file=sys.stderr)

    # Summary
    high = sum(1 for pairs in groups.values() for _, _, _, j, _ in pairs if j >= 0.8)
    med = sum(1 for pairs in groups.values() for _, _, _, j, _ in pairs if 0.5 <= j < 0.8)
    prefix_count = sum(1 for pairs in groups.values() for _, _, _, _, p in pairs if p)
    print(f"High similarity (J>=0.8): {high}", file=sys.stderr)
    print(f"Medium similarity (J>=0.5): {med}", file=sys.stderr)
    print(f"Prefix pairs: {prefix_count}", file=sys.stderr)


if __name__ == "__main__":
    main()
