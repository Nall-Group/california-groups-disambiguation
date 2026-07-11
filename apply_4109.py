import json

P='2_webapp/org_clusters_crosswalk.json'
d=json.load(open(P))
clusters=d['clusters']

def add_alt(children, name):
    if any(ch.get('name')==name for ch in children):
        print("  already present:", name); return False
    children.append({"name":name,"relationship":"alternate_spelling"})
    print("  ADDED:", name); return True

# 1) alt under canonical "The Art and Creative Materials Institute, Inc"
for c in clusters:
    if c['canonical']=="The Art and Creative Materials Institute, Inc":
        print("The Art and Creative Materials Institute, Inc")
        add_alt(c.setdefault('children',[]), "The Art & Creative Materials Institute, Inc")
        break
else:
    raise SystemExit("MISSING canonical: Art and Creative Materials")

# 2) alt under canonical "Assembly Health Committee"
for c in clusters:
    if c['canonical']=="Assembly Health Committee":
        print("Assembly Health Committee")
        add_alt(c.setdefault('children',[]), "The Assembly Health Committee")
        break
else:
    raise SystemExit("MISSING canonical: Assembly Health Committee")

# 3) alt under chapter node "Automobile Club of Southern California" (parent AAA)
done=False
for c in clusters:
    if c['canonical']=="American Automobile Association":
        for ch in c['children']:
            if ch['name']=="Automobile Club of Southern California":
                print("Automobile Club of Southern California (chapter of AAA)")
                add_alt(ch.setdefault('children',[]), "The Automobile Club of Southern California Association")
                done=True
        break
if not done:
    raise SystemExit("MISSING chapter node: Automobile Club of Southern California")

json.dump(d, open(P,'w'), ensure_ascii=False, indent=1)
print("WROTE", P)
