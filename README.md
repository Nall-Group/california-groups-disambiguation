# California Groups Disambiguation

![Canonical Orgs](https://img.shields.io/badge/dynamic/json?url=https%3A%2F%2Fraw.githubusercontent.com%2FNall-Group%2Fcalifornia-groups-disambiguation%2Fmain%2Fstats.json&query=%24.canonical_orgs&label=canonical%20orgs&color=blue&cacheSeconds=0)
![Total in Crosswalk](https://img.shields.io/badge/dynamic/json?url=https%3A%2F%2Fraw.githubusercontent.com%2FNall-Group%2Fcalifornia-groups-disambiguation%2Fmain%2Fstats.json&query=%24.total_in_crosswalk&label=total%20in%20crosswalk&color=green&cacheSeconds=0)
![Not in Crosswalk](https://img.shields.io/badge/dynamic/json?url=https%3A%2F%2Fraw.githubusercontent.com%2FNall-Group%2Fcalifornia-groups-disambiguation%2Fmain%2Fstats.json&query=%24.not_in_crosswalk&label=not%20in%20crosswalk&color=orange&cacheSeconds=0)
![Invalid Entries](https://img.shields.io/badge/dynamic/json?url=https%3A%2F%2Fraw.githubusercontent.com%2FNall-Group%2Fcalifornia-groups-disambiguation%2Fmain%2Fstats.json&query=%24.invalid_entries&label=invalid%20entries&color=red&cacheSeconds=0)

## 1_gather_orgs

Here we have gathered a canonical list of groups in California that support or oppose legislation related to housing

## 2_webapp

We also provide a disambiguation tool hosted on [Ruth todo - fill in github web app link once it's ready]

Future feature: add fuzzy search feature to disambiguation tool

## data cleaning

directory: `org_name_for_cleaning/`

### Importing new Leginfo metadata

To pull fresh organization names out of the [Leginfo](https://github.com/Nall-Group/leginfo) bill metadata (the support/opposition/sponsor stance columns), resolve them against the crosswalk, and route whatever's left to the right invalidity CSV, follow the runnable playbook in [`LEGINFO_IMPORT.md`](LEGINFO_IMPORT.md). It ensures every org that supported or opposed a bill ends up either in the crosswalk at the correct hierarchy level or in the correct `leginfo_*.csv` file — no org string is ever discarded.

## Managing Robot RAs (Claude Code Workers)

This project supports parallel task execution using multiple Claude Code sessions as "Research Assistants" (RAs), coordinated through `TASKS.md` and `QUESTIONS.md`. See `CLAUDE.md` for the full protocol.

### Setup

You need three terminal sessions running Claude Code in this directory:

1. **Management Assistant** — coordinates between you and the workers
2. **Worker RA sessions** (2+) — do the actual tasks

### Starting the Management Assistant

```
claude
```

Then prompt:

```
You are the Management Assistant. Read CLAUDE.md and follow the Management Assistant instructions. Poll QUESTIONS.md every 10 seconds for unanswered RA questions and present them to me. Also report task status (done/in progress/blocked/not started) with each check. When I send you a task description, add it to TASKS.md.
```

### Starting Worker RAs

Open separate terminals, run `claude` in each, and prompt:

```
You are RA-Alpha. Read CLAUDE.md and follow the Worker RA instructions. Pick a task from TASKS.md and get started.
```

```
You are RA-Beta. Read CLAUDE.md and follow the Worker RA instructions. Pick a task from TASKS.md and get started.
```

Use a different name for each session (RA-Alpha, RA-Beta, RA-Gamma, etc.).

### Day-to-Day Workflow

1. **Adding tasks**: Describe tasks to the Management Assistant in plain language. It formats and adds them to `TASKS.md`.
2. **Answering questions**: The manager polls `QUESTIONS.md` and surfaces RA questions to you. Give your answer and the manager writes it back.
3. **Status checks**: Ask the manager for a status update — it reads `TASKS.md` and summarizes.
4. **Blocked tasks**: When an RA gets stuck, it posts a question, unassigns itself, and moves on. Any RA can pick up a blocked task once the question is answered.

### Key Files

| File | Purpose |
|------|---------|
| `CLAUDE.md` | Full protocol for both roles |
| `TASKS.md` | Task list and write queue |
| `QUESTIONS.md` | Q&A between RAs and human |

## Crosswalk Relationship Types

- canonical
- alternate_spelling
- chapter
- renamed - org was renamed (e.g., Sierra Club Legal Defense Fund → Earthjustice)
- pre-merge - org that later merged into this one (e.g., AFL is a pre-merge of AFL-CIO)
- previously_known_as - prior name of the org

## Cleaning Patterns

`cleaning_patterns.txt` contains regex patterns that should be removed from the end of organization names. These are metadata about bill relationships (e.g., sponsor status), not part of the actual org name.

Patterns include:
- (CO-SPONSOR), (SPONSOR), (Sponsors)
- (Co-Source)
- (PRIOR VERSION)
- (OPPOSE), (OPPOSE UNLESS AMENDED)
- ("In Concept")
- (Principal Co-Sponsor), (Co-Sponsors)

## Cleaning Pipeline

The cleaning/deduplication/stats pipeline is documented in `CLAUDE.md`
("Cleaning & deduplication pipeline"). Run these in order whenever the crosswalk
or `cleaning_patterns.txt` changes:

1. `python3 scripts/clean_crosswalk.py` — apply `cleaning_patterns.txt`, dedup children, merge clusters that normalize identically.
2. `python3 scripts/regenerate_org_subsets.py` — re-check all org names against the crosswalk and redistribute the `org_name_for_cleaning/` CSVs.
3. `python3 generate_stats.py` — update `stats.json`.

## Invalid Organization Names

`org_names_invalid.csv` contains placeholder values and metadata fragments that are not actual organization names. These include:
- "None on file", "NONE RECEIVED", "None Known", "None to date", etc.
- "END", "Inc", "Oppose", "Unknown"
- Empty strings

