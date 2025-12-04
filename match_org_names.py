#!/usr/bin/env python3
"""
Match organization names against the crosswalk.

This script can:
1. Match individual org names from command line arguments
2. Match org names from a CSV file
3. Match org names from stdin (one per line)

Usage:
    # Match individual names from command line
    python match_org_names.py "SEIU" "S.E.I.U." "Sierra Club (co-sponsor)"

    # Match names from a CSV file (reads 'org_name' column by default)
    python match_org_names.py --input orgs.csv --output matched.csv

    # Match names from stdin
    echo "SEIU" | python match_org_names.py --stdin

    # Specify a different column name
    python match_org_names.py --input orgs.csv --column "organization" --output matched.csv
"""

import argparse
import csv
import sys
from pathlib import Path

from org_matching_utils import CrosswalkMatcher, MatchResult


def match_from_args(matcher: CrosswalkMatcher, names: list[str], verbose: bool = True):
    """Match org names provided as command line arguments."""
    for name in names:
        result = matcher.match(name)
        if verbose:
            status = "✓" if result.is_match else "✗"
            print(f"{status} {repr(name)}")
            if result.cleaned_input != name:
                print(f"   cleaned: {repr(result.cleaned_input)}")
            if result.is_match:
                print(f"   -> {result.match_type} match")
                print(f"   canonical: {repr(result.canonical)}")
            print()
        else:
            if result.is_match:
                print(f"{name}\t{result.match_type}\t{result.canonical}")
            else:
                print(f"{name}\tno_match\t")


def match_from_stdin(matcher: CrosswalkMatcher, verbose: bool = True):
    """Match org names from stdin (one per line)."""
    names = [line.strip() for line in sys.stdin if line.strip()]
    match_from_args(matcher, names, verbose)


def match_from_csv(
    matcher: CrosswalkMatcher,
    input_path: Path,
    output_path: Path,
    column: str = "org_name"
):
    """Match org names from a CSV file and write results to a new CSV."""
    with open(input_path, 'r', encoding='utf-8') as infile:
        reader = csv.DictReader(infile)

        if column not in reader.fieldnames:
            print(f"Error: Column '{column}' not found in {input_path}")
            print(f"Available columns: {reader.fieldnames}")
            sys.exit(1)

        # Output columns: original columns + match results
        output_fields = list(reader.fieldnames) + [
            'in_crosswalk', 'match_type', 'canonical_name', 'cleaned_name'
        ]

        with open(output_path, 'w', encoding='utf-8', newline='') as outfile:
            writer = csv.DictWriter(outfile, fieldnames=output_fields)
            writer.writeheader()

            match_count = 0
            total_count = 0

            for row in reader:
                total_count += 1
                org_name = row[column]

                if org_name:
                    result = matcher.match(org_name)
                    row['in_crosswalk'] = 'yes' if result.is_match else 'no'
                    row['match_type'] = result.match_type or ''
                    row['canonical_name'] = result.canonical or ''
                    row['cleaned_name'] = result.cleaned_input

                    if result.is_match:
                        match_count += 1
                else:
                    row['in_crosswalk'] = ''
                    row['match_type'] = ''
                    row['canonical_name'] = ''
                    row['cleaned_name'] = ''

                writer.writerow(row)

    print(f"Processed {total_count} rows")
    print(f"Matched: {match_count} ({100*match_count/total_count:.1f}%)")
    print(f"Not matched: {total_count - match_count}")
    print(f"Output written to {output_path}")


def main():
    parser = argparse.ArgumentParser(
        description="Match organization names against the crosswalk.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__
    )

    parser.add_argument(
        'names',
        nargs='*',
        help="Organization names to match (from command line)"
    )

    parser.add_argument(
        '--input', '-i',
        type=Path,
        help="Input CSV file with org names"
    )

    parser.add_argument(
        '--output', '-o',
        type=Path,
        help="Output CSV file for matched results"
    )

    parser.add_argument(
        '--column', '-c',
        default='org_name',
        help="Column name containing org names (default: 'org_name')"
    )

    parser.add_argument(
        '--stdin',
        action='store_true',
        help="Read org names from stdin (one per line)"
    )

    parser.add_argument(
        '--quiet', '-q',
        action='store_true',
        help="Quiet output (tab-separated: name, match_type, canonical)"
    )

    parser.add_argument(
        '--no-clean',
        action='store_true',
        help="Don't apply cleaning patterns before matching"
    )

    args = parser.parse_args()

    # Initialize matcher
    matcher = CrosswalkMatcher(apply_cleaning=not args.no_clean)

    # Determine mode
    if args.input:
        # CSV mode
        if not args.output:
            args.output = args.input.with_stem(args.input.stem + '_matched')
        match_from_csv(matcher, args.input, args.output, args.column)

    elif args.stdin:
        # Stdin mode
        match_from_stdin(matcher, verbose=not args.quiet)

    elif args.names:
        # Command line args mode
        match_from_args(matcher, args.names, verbose=not args.quiet)

    else:
        # No input provided - show help
        parser.print_help()
        print("\nExamples:")
        print('  python match_org_names.py "SEIU" "Sierra Club"')
        print('  python match_org_names.py --input orgs.csv --output matched.csv')
        print('  echo "SEIU" | python match_org_names.py --stdin')


if __name__ == "__main__":
    main()
