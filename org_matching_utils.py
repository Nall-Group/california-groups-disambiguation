#!/usr/bin/env python3
"""
Utility functions for matching organization names against the crosswalk.

This module provides shared functionality for:
- Loading and applying cleaning patterns
- Normalizing names for matching (case, punctuation, spacing)
- Loading the crosswalk into efficient lookup structures
- Matching org names against the crosswalk

Usage:
    from org_matching_utils import CrosswalkMatcher

    matcher = CrosswalkMatcher()
    result = matcher.match("S.E.I.U.")
    # result: MatchResult(is_match=True, match_type='normalized', canonical='SEIU', ...)
"""

import json
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Optional, List, Tuple

# Default paths (relative to this file's directory)
_BASE_DIR = Path(__file__).parent
DEFAULT_CROSSWALK_PATH = _BASE_DIR / "2_webapp" / "org_clusters_crosswalk.json"
DEFAULT_CLEANING_PATTERNS_PATH = _BASE_DIR / "cleaning_patterns.txt"


@dataclass
class MatchResult:
    """Result of matching an org name against the crosswalk."""
    is_match: bool
    match_type: Optional[str]  # 'exact', 'normalized', or None
    canonical: Optional[str]   # The canonical name if matched
    cleaned_input: str         # The input after cleaning patterns applied
    normalized_input: str      # The input after normalization


def load_cleaning_patterns(path: Path = DEFAULT_CLEANING_PATTERNS_PATH) -> List[re.Pattern]:
    """
    Load regex patterns from cleaning_patterns.txt.

    These patterns remove metadata annotations like (co-sponsor), [prior version], etc.
    """
    patterns = []
    with open(path, 'r', encoding='utf-8') as f:
        for line in f:
            line = line.strip()
            # Skip empty lines and comments
            if not line or line.startswith('#'):
                continue
            try:
                patterns.append(re.compile(line))
            except re.error as e:
                print(f"Warning: Invalid regex pattern '{line}': {e}")
    return patterns


def fix_mismatched_brackets(name: str) -> str:
    """
    Fix mismatched brackets in an org name.

    Handles five categories:
    - Category 5: Leading fragment like "2007) IFPTE" or "10)Professional..." -> strip leading garbage
    - Category 2: Metadata remnants like "Org (Oppose" -> strip from unclosed bracket to end
    - Category 1: Missing closing bracket like "Org (ACRONYM" -> add closing bracket
    - Category 3: Stray closing bracket with acronym like "Org ACRONYM)" -> insert opening bracket
    - Category 4: Stray closing bracket other like "Org)" -> remove stray bracket
    """
    # Quick check: are brackets already balanced?
    if (name.count('(') == name.count(')') and
            name.count('[') == name.count(']')):
        return name

    # Category 5: Leading fragment — starts with optional digits then )
    # e.g. "2007) IFPTE", "10)Professional and Technical Engineers"
    m = re.match(r'^[^(]*?\d*\)\s*', name)
    if m and name.count(')') > name.count('('):
        stripped = name[m.end():]
        if stripped:
            return fix_mismatched_brackets(stripped)

    # --- More opening parens than closing: unclosed ( or [ ---
    if name.count('(') > name.count(')'):
        # Category 2: Metadata remnants — unclosed bracket contains metadata keywords
        # Check from the LAST unclosed ( to end of string
        metadata_pattern = re.compile(
            r'\s*\('
            r'(?:'
            r'oppose'
            r'|co\b'
            r'|per\s'
            r'|["\u201c]'
            r'|but\b'
            r'|(?:january|february|march|april|may|june|july|august|september|october|november|december)\b'
            r'|sponsor'
            r'|source'
            r'|\d{4}'
            r')',
            re.IGNORECASE
        )
        if metadata_pattern.search(name):
            # Strip from the unclosed ( that matches metadata to end of string
            # Find the position of the metadata match
            match = metadata_pattern.search(name)
            cleaned = name[:match.start()].strip()
            if cleaned:
                return cleaned

        # Category 1: Missing closing bracket — add it
        # e.g. "Org (ACRONYM" -> "Org (ACRONYM)"
        return name + ')'

    if name.count('[') > name.count(']'):
        # Category 1 for square brackets
        return name + ']'

    # --- More closing parens than opening: stray ) ---
    if name.count(')') > name.count('('):
        # Category 3: Missing opening bracket before acronym
        # e.g. "California Senior Legislature CSL)" -> "California Senior Legislature (CSL)"
        # Only match when preceding text has lowercase letters (mixed-case name + acronym)
        # to avoid false positives on all-caps text like "AND SIMI VALLEY)"
        m = re.search(r'\b([A-Z]{2,})\)\s*$', name)
        if m:
            prefix = name[:m.start()].rstrip()
            if re.search(r'[a-z]', prefix):
                acronym = m.group(1)
                return f"{prefix} ({acronym})"

        # Category 4: Stray closing bracket — just remove it
        # Remove the last unmatched )
        # Find and remove one stray )
        result = []
        extra_closes = name.count(')') - name.count('(')
        for ch in reversed(name):
            if ch == ')' and extra_closes > 0:
                extra_closes -= 1
                continue
            result.append(ch)
        cleaned = ''.join(reversed(result)).strip()
        return re.sub(r'\s+', ' ', cleaned)

    if name.count(']') > name.count('['):
        # Remove stray ]
        result = []
        extra_closes = name.count(']') - name.count('[')
        for ch in reversed(name):
            if ch == ']' and extra_closes > 0:
                extra_closes -= 1
                continue
            result.append(ch)
        cleaned = ''.join(reversed(result)).strip()
        return re.sub(r'\s+', ' ', cleaned)

    return name


def clean_org_name(name: str, patterns: List[re.Pattern]) -> str:
    """
    Apply cleaning patterns to remove metadata annotations from org name,
    then fix any mismatched brackets.

    Examples:
        "Sierra Club (co-sponsor)" -> "Sierra Club"
        "(sponsor) ACLU" -> "ACLU"
        "Association of Bay Area Governments (ABAG" -> "Association of Bay Area Governments (ABAG)"
    """
    cleaned = name
    for pattern in patterns:
        cleaned = pattern.sub(' ', cleaned)
    # Collapse multiple spaces and strip
    cleaned = re.sub(r'\s+', ' ', cleaned).strip()
    # Fix mismatched brackets
    cleaned = fix_mismatched_brackets(cleaned)
    # Unwrap names entirely wrapped in parentheses: "(AFSCME)" -> "AFSCME"
    if cleaned.startswith('(') and cleaned.endswith(')') and cleaned.count('(') == 1 and cleaned.count(')') == 1:
        cleaned = cleaned[1:-1].strip()
    return cleaned


def normalize_for_matching(name: str) -> str:
    """
    Normalize a name for matching purposes.

    - Lowercases
    - Removes all punctuation EXCEPT % (which is semantically meaningful)
    - Collapses whitespace

    Examples:
        "California Teachers' Association" -> "california teachers association"
        "S.E.I.U." -> "seiu"
        "ACME, Inc." -> "acme inc"
        "100% Campaign" -> "100% campaign"
    """
    s = name.lower()
    # Remove all punctuation EXCEPT %
    s = re.sub(r'[^\w\s%]', '', s)
    # Collapse whitespace
    s = re.sub(r'\s+', ' ', s).strip()
    return s


class CrosswalkMatcher:
    """
    Efficient matcher for organization names against the crosswalk.

    Loads the crosswalk once and provides fast lookups with:
    - Case-insensitive exact matching
    - Normalized matching (ignores punctuation/spacing differences)
    """

    def __init__(
        self,
        crosswalk_path: Path = DEFAULT_CROSSWALK_PATH,
        cleaning_patterns_path: Path = DEFAULT_CLEANING_PATTERNS_PATH,
        apply_cleaning: bool = True
    ):
        """
        Initialize the matcher.

        Args:
            crosswalk_path: Path to org_clusters_crosswalk.json
            cleaning_patterns_path: Path to cleaning_patterns.txt
            apply_cleaning: Whether to apply cleaning patterns before matching
        """
        self.crosswalk_path = Path(crosswalk_path)
        self.cleaning_patterns_path = Path(cleaning_patterns_path)
        self.apply_cleaning = apply_cleaning

        # Load cleaning patterns
        self._cleaning_patterns = load_cleaning_patterns(self.cleaning_patterns_path)

        # Load crosswalk into lookup structures
        self._exact_names: set = set()  # Uppercased names for exact lookup
        self._normalized_to_canonical: dict = {}  # Normalized -> canonical mapping
        self._load_crosswalk()

    def _load_crosswalk(self):
        """Load the crosswalk JSON into lookup structures."""
        with open(self.crosswalk_path, 'r', encoding='utf-8') as f:
            data = json.load(f)

        for cluster in data['clusters']:
            canonical = cluster['canonical']

            # Add canonical name
            self._exact_names.add(canonical.strip().upper())
            norm_canonical = normalize_for_matching(canonical)
            if norm_canonical:
                self._normalized_to_canonical[norm_canonical] = canonical

            # Add all children (recursively)
            def _walk_children(children):
                for child in children:
                    # Tolerate children malformed with a "canonical" key (see Q52)
                    child_name = child['name'] if 'name' in child else child['canonical']
                    self._exact_names.add(child_name.strip().upper())
                    norm_child = normalize_for_matching(child_name)
                    if norm_child and norm_child not in self._normalized_to_canonical:
                        self._normalized_to_canonical[norm_child] = canonical
                    if 'children' in child:
                        _walk_children(child['children'])

            _walk_children(cluster.get('children', []))

    @property
    def exact_name_count(self) -> int:
        """Number of exact names in the crosswalk."""
        return len(self._exact_names)

    @property
    def normalized_name_count(self) -> int:
        """Number of normalized names in the crosswalk."""
        return len(self._normalized_to_canonical)

    def clean(self, name: str) -> str:
        """Apply cleaning patterns to a name."""
        return clean_org_name(name, self._cleaning_patterns)

    def normalize(self, name: str) -> str:
        """Normalize a name for matching."""
        return normalize_for_matching(name)

    def match(self, org_name: str) -> MatchResult:
        """
        Match an org name against the crosswalk.

        Process:
        1. Apply cleaning patterns (if enabled)
        2. Try exact match (case-insensitive)
        3. Try normalized match (punctuation/spacing insensitive)

        Args:
            org_name: The organization name to match

        Returns:
            MatchResult with match details
        """
        # Apply cleaning if enabled
        if self.apply_cleaning:
            cleaned = self.clean(org_name)
        else:
            cleaned = org_name.strip()

        normalized = self.normalize(cleaned)

        # Try exact match (case-insensitive)
        if cleaned.upper() in self._exact_names:
            canonical = self._normalized_to_canonical.get(normalized, cleaned)
            return MatchResult(
                is_match=True,
                match_type='exact',
                canonical=canonical,
                cleaned_input=cleaned,
                normalized_input=normalized
            )

        # Try normalized match
        if normalized in self._normalized_to_canonical:
            return MatchResult(
                is_match=True,
                match_type='normalized',
                canonical=self._normalized_to_canonical[normalized],
                cleaned_input=cleaned,
                normalized_input=normalized
            )

        # No match
        return MatchResult(
            is_match=False,
            match_type=None,
            canonical=None,
            cleaned_input=cleaned,
            normalized_input=normalized
        )

    def match_batch(self, org_names: List[str]) -> List[Tuple[str, MatchResult]]:
        """
        Match multiple org names against the crosswalk.

        Args:
            org_names: List of organization names to match

        Returns:
            List of (original_name, MatchResult) tuples
        """
        return [(name, self.match(name)) for name in org_names]


# Convenience functions for simple usage
_default_matcher: Optional[CrosswalkMatcher] = None


def get_default_matcher() -> CrosswalkMatcher:
    """Get or create the default matcher instance."""
    global _default_matcher
    if _default_matcher is None:
        _default_matcher = CrosswalkMatcher()
    return _default_matcher


def match_org_name(org_name: str) -> MatchResult:
    """
    Match a single org name using the default matcher.

    Convenience function for simple usage.
    """
    return get_default_matcher().match(org_name)


if __name__ == "__main__":
    # Demo usage
    print("Loading crosswalk matcher...")
    matcher = CrosswalkMatcher()
    print(f"Loaded {matcher.exact_name_count} exact names")
    print(f"Loaded {matcher.normalized_name_count} normalized names")

    # Test some matches
    test_names = [
        "SEIU",
        "S.E.I.U.",
        "SEIU -",
        "California Teachers Association",
        "California Teachers' Association",
        "Sierra Club (co-sponsor)",
        "Some Random Org That Does Not Exist",
    ]

    print("\nTest matches:")
    for name in test_names:
        result = matcher.match(name)
        status = "✓" if result.is_match else "✗"
        print(f"  {status} {repr(name)}")
        if result.is_match:
            print(f"      -> {result.match_type} match, canonical: {repr(result.canonical)}")
        if result.cleaned_input != name:
            print(f"      cleaned: {repr(result.cleaned_input)}")
