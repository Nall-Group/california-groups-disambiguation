import glob, os
TMP=os.environ.get("TMPDIR","/tmp")
files=sorted(glob.glob(TMP+"/w*_*.out"))
buckets={
 "consolidate":[], "new_canonicals":[], "new_chapters":[], "individuals":[],
 "partial":[], "conjoined":[], "narrative":[], "parens":[], "dates":[],
 "not_capitalized":[], "invalid":[],
}
seen=set()
for fn in files:
    for ln in open(fn):
        ln=ln.rstrip("\n")
        if not ln.strip(): continue
        p=ln.split("\t")
        org=p[0].strip()
        disp=(p[1].strip() if len(p)>1 else "")
        target=(p[2].strip() if len(p)>2 else "-")
        group=(p[3].strip() if len(p)>3 else "")
        key=(org,disp)
        if key in seen: continue
        seen.add(key)
        d=disp.upper()
        if d.startswith("CONSOLIDATE"): buckets["consolidate"].append("%s\t%s\t%s"%(org,target,group))
        elif d=="NEW_CANONICAL": buckets["new_canonicals"].append(org)
        elif d=="NEW_CHAPTER": buckets["new_chapters"].append("%s\t%s"%(org,target))
        elif d=="INVALID:INDIVIDUALS": buckets["individuals"].append(org)
        elif d=="INVALID:PARTIAL": buckets["partial"].append(org)
        elif d=="INVALID:CONJOINED": buckets["conjoined"].append(org)
        elif d=="INVALID:NARRATIVE": buckets["narrative"].append(org)
        elif d=="INVALID:PARENS": buckets["parens"].append(org)
        elif d=="INVALID:DATES_PHONE": buckets["dates"].append(org)
        elif d=="INVALID:NOT_CAPITALIZED": buckets["not_capitalized"].append(org)
        elif d.startswith("INVALID"): buckets["invalid"].append(org)
        else: buckets["invalid"].append(org)  # unknown -> invalid bin for review
ext={"consolidate":"tsv","new_chapters":"tsv"}
for name,rows in buckets.items():
    fn="gaps_master_%s.%s"%(name, ext.get(name,"txt"))
    open(fn,"w").write("\n".join(rows)+("\n" if rows else ""))
print("rebuilt from %d .out files (%d unique dispositions):"%(len(files),len(seen)))
for name,rows in buckets.items(): print("  gaps_master_%s: %d"%(name,len(rows)))
