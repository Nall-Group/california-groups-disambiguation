export const meta = {
  name: 'leginfo-resolution-scan',
  description: 'Diagnose unmatched leginfo orgs (org-vs-prose, triage, crosswalk placement) — one schema agent per ~15-item batch',
  phases: [{ title: 'Diagnose', detail: 'one diagnosis agent per batch' }],
}

const SCHEMA = {
  type: 'object',
  properties: {
    diagnoses: {
      type: 'array',
      items: {
        type: 'object',
        properties: {
          original: { type: 'string' },
          count: { type: 'number' },
          judgment: { type: 'string', enum: ['org_name', 'prose'] },
          classification: { type: 'string', enum: ['valid', 'already_in_crosswalk', 'invalid', 'individual', 'partial', 'conjoined'] },
          extracted_orgs: { type: 'array', items: { type: 'string' } },
          target_csv: { type: ['string', 'null'] },
          crosswalk_placement: {
            type: 'object',
            properties: {
              canonical: { type: ['string', 'null'] },
              relation: { type: ['string', 'null'] },
              attach_to_node: { type: ['string', 'null'] },
            },
            required: ['canonical', 'relation', 'attach_to_node'],
          },
          notes: { type: 'string' },
          delete_from_crosswalk: { type: 'array', items: { type: 'string' } },
        },
        required: ['original', 'count', 'judgment', 'classification', 'extracted_orgs', 'target_csv', 'crosswalk_placement', 'notes', 'delete_from_crosswalk'],
      },
    },
  },
  required: ['diagnoses'],
}

function promptFor(batch) {
  return `You are a diagnosis sub-agent for the leginfo import resolution scan (step 2). DO NOT EDIT ANY FILES and do NOT open any project docs — every rule you need is inlined below. Diagnose only; return the structured object.

KEY CROSSWALK RULES (inlined): (a) SEARCH THE CROSSWALK FIRST — the org is usually already present as a canonical, chapter, or alternate spelling; use relation "new_canonical" ONLY if it genuinely is nowhere. (b) Place at the CORRECT hierarchy level — alternate_spelling vs chapter vs alt_of_chapter (a city Chamber of Commerce → chapter under the U.S./California Chamber tree; an AFSCME/SEIU/union local → under that union; a Mayor/Sheriff/City Attorney/City Manager → under that city/county office canonical). (c) PRESERVE every real org name — a dirty, truncated, or OCR-typo spelling becomes an alternate_spelling of the clean org, never discarded. (d) Do NOT strip location/chapter suffixes ("Inner City Law Center, Los Angeles" may be a chapter) or "dba" names; DO strip bill/position metadata (SB ###, "(sponsor)", "in support", "(previous version)", dates, counts). (e) Out-of-state orgs that lobby the CA legislature are legitimate — keep them. (f) Truncated/ambiguous fragments: search the crosswalk AND the web; classify "partial" ONLY if still ambiguous after both.

Your batch is a CSV file (columns: org_name,count — NO header, org_name may be quoted). Read every row from: ${batch.file}
Diagnose ALL of its rows (there are about 30). Echo each "original" EXACTLY as it appears in the file and the same count.

For EACH item, read it individually and decide:

1. ORG NAME or NARRATIVE PROSE? Prose = a sentence/fragment (e.g. "we strongly", "While we", "will", "supported by the board", "In the individual market, the ACA sought to..."). If prose: grep the exact string in \`/Users/ruthgracewong/leginfo/extract_all_leginfo_metadata/leginfo_metadata.csv\` to recover the whole cell, then extract the real org(s) into extracted_orgs. If the prose names a real org → classification "valid" (or "conjoined" if several); if it names NO org → classification "invalid", target_csv "org_names_invalid.csv".

2. CLEAN & SEARCH THE CROSSWALK. Crosswalk = \`/Users/ruthgracewong/california-groups-disambiguation/2_webapp/org_clusters_crosswalk.json\` (huge — use \`grep -i\` for canonical/name strings; try acronyms, spellings, and the PARENT org). Find whether the org (or its parent) is already present and under what exact node.

3. TRIAGE (conjoined FIRST):
   - conjoined: multiple orgs mashed together (e.g. "A coalition ... including the California Chamber of Commerce") → list the real orgs in extracted_orgs, classification "conjoined". Do NOT set target_csv.
   - individual: a person who is NOT a leader of an identifiable org → classification "individual", target_csv "org_names_that_are_actually_individuals.csv". LEADERSHIP EXCEPTION (these are "valid" alt-spellings of the org, NOT individuals): Mayor / City Attorney / District Attorney / Sheriff / Chief of Police / President / CEO / Superintendent / Chair / Director-of-whole-org. Councilmember / Supervisor / Commissioner / Trustee / Board member = individual.
   - partial: truncated/ambiguous fragment → try to disambiguate (crosswalk + web); only if still ambiguous → classification "partial", target_csv "org_names_partial.csv".
   - invalid: not an org at all → classification "invalid", target_csv "org_names_invalid.csv".
   - valid: a single clean real org to ADD to the crosswalk. Set crosswalk_placement: canonical (existing or new), relation one of alternate_spelling|chapter|alt_of_chapter|new_canonical|already_present, and attach_to_node = the EXACT existing node name to attach under (or null for new_canonical). Search first — chambers of commerce nest under the California/U.S. Chamber tree; AFSCME union locals nest under AFSCME; a Mayor/City Attorney/Sheriff attaches under the CITY/COUNTY office canonical.

PROSE THAT NAMES A REAL ORG: set judgment "prose", extracted_orgs to the org name(s), and classification "already_in_crosswalk" if that org is already present (relation "already_present") or "valid" if it is absent (give its placement). The driver discards the prose string and records a prose→org rewrite for step 4 — you do NOT keep the prose.

ORG NAME ALREADY PRESENT UNDER A DIFFERENT SPELLING: use classification "valid" (or "already_in_crosswalk") with the placement of the existing canonical/node — the driver adds THIS exact leginfo spelling as an alternate_spelling so it is preserved. Never leave it unrepresented.

delete_from_crosswalk: while grepping, if you discover a node ALREADY IN the crosswalk that is itself accidental NARRATIVE PROSE (a sentence/fragment wrongly added as an org, e.g. a node literally named "we strongly support this bill"), put that EXACT node string into delete_from_crosswalk so the driver files an RA task to DELETE it from the crosswalk. Do NOT route such nodes to a CSV. Normally this array is [].

For non-valid, non-conjoined items set extracted_orgs to [] and crosswalk_placement fields to null. For "valid", set extracted_orgs to [the clean org name]. Set delete_from_crosswalk to [] unless you actually found accidental prose in the crosswalk. Always echo "original" EXACTLY as given and the same count. Actually grep the crosswalk — do not guess.`
}

phase('Diagnose')
const batches = typeof args === 'string' ? JSON.parse(args) : args
const results = await parallel(
  batches.map(b => () =>
    agent(promptFor(b), { label: `batch ${b.batch}`, phase: 'Diagnose', schema: SCHEMA, model: 'opus' })
      .then(r => ({ batch: b.batch, diagnoses: (r && r.diagnoses) || null }))
  )
)
return results