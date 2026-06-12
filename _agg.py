import sys, glob, os
# Usage: python3 _agg.py wNN   -> rebuild all masters from every .out file (idempotent, separates narrative/parens/dates),
# then advance the needs_audit cursor by removing wave wNN's chunk orgs.
wave=sys.argv[1] if len(sys.argv)>1 else None
TMP=os.environ.get("TMPDIR","/tmp")
files=sorted(glob.glob(TMP+"/w*_*.out"))
buckets={k:[] for k in ["consolidate","new_canonicals","new_chapters","individuals","partial",
 "conjoined","narrative","parens","dates","not_capitalized","invalid"]}
seen=set()
for fn in files:
    for ln in open(fn):
        ln=ln.rstrip("\n")
        if not ln.strip(): continue
        p=ln.split("\t"); org=p[0].strip()
        disp=(p[1].strip() if len(p)>1 else ""); target=(p[2].strip() if len(p)>2 else "-"); group=(p[3].strip() if len(p)>3 else "")
        if (org,disp) in seen: continue
        seen.add((org,disp)); d=disp.upper()
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
        else: buckets["invalid"].append(org)
ext={"consolidate":"tsv","new_chapters":"tsv"}
for name,rows in buckets.items():
    open("gaps_master_%s.%s"%(name,ext.get(name,"txt")),"w").write("\n".join(rows)+("\n" if rows else ""))
# advance cursor for this wave
if wave:
    waveorgs=set()
    for fn in glob.glob(TMP+"/%s_*.out"%wave):
        for ln in open(fn):
            if ln.strip(): waveorgs.add(ln.split("\t")[0].strip())
    na=[l.rstrip("\n") for l in open("gaps_needs_audit.txt") if l.strip()]
    keep=[o for o in na if o not in waveorgs]
    open("gaps_needs_audit.txt","w").write("\n".join(keep)+"\n")
    print("%s: wave orgs=%d  needs_audit remaining=%d"%(wave,len(waveorgs),len(keep)))
tot=sum(len(v) for v in buckets.values())
print("masters (%d total): "%tot + " ".join("%s=%d"%(k,len(v)) for k,v in buckets.items()))
