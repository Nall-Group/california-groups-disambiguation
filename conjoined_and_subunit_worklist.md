# Conjoined-Split + Sub-Unit-Nesting Worklist

Source: MA-2 conjoined-verification wave (2026-07-04). 8 agents classified 507 conjoined-*candidate* top-level canonicals as **SPLIT** (truly conjoined = 2+ independent orgs), **NEST** (one org + its own sub-unit → nest under parent), or **LEAVE** (single org; not actioned here). Only SPLIT and NEST are listed. Tasks 38xx execute this file. For every action: verify against the live JSON, merge-append (never replace), preserve children, remove the emptied top-level canonical.

---

## PART A — CONJOINED SPLIT (route each string to `org_names_conjoined.csv`)
Workflow per entry: (1) parse the component orgs; (2) confirm EACH component already exists in the crosswalk (search first — add as a new canonical only if genuinely missing); (3) remove the conjoined top-level canonical from the JSON; (4) move its row (org_name + count) from `org_name_subsets_for_cleaning/org_names_in_crosswalk.csv` to `org_name_subsets_for_cleaning/org_names_conjoined.csv`. Run regenerate_org_subsets + generate_stats.

1. `American Heart Association/American Stroke Association (AHA/ASA)` → American Heart Association ; American Stroke Association (ASA is an AHA division — may instead fold as alt of AHA per task 3820; RA judgment)
2. `California Narcotics Officers Doris Tate Crime Victims Bureau Orange County Chiefs' and Sheriff's Association Neil J. Purcell` → California Narcotic Officers' Association ; Doris Tate Crime Victims Bureau ; Orange County Chiefs' and Sheriffs Association ; **Neil J. Purcell → individuals CSV** (person)
3. `California Nurse Association/National Nurses Organizing Committee (CAN/NNOC)` → California Nurses Association ; National Nurses Organizing Committee (both exist; note "Nurse"→"Nurses" typo)
4. `City of Poway, Rails to Trails Conservancy` → City of Poway ; Rails to Trails Conservancy
5. `District and the Etna Union High School District (districts)` → **PARTIAL**: leading "District" is an unidentifiable fragment; keep only `Etna Union High School District` (confirm it exists), route the conjoined string to conjoined CSV (or partial CSV if the fragment can't be resolved)
6. `University of California, San Francisco VA Medical Center` → University of California, San Francisco ; San Francisco VA Medical Center (federal VA hospital — distinct affiliated entity)
7. `SNAP INC. Institute for Technology & Education - California State University Dominguez Hills` → SNAP Inc. Institute for Technology & Education ; California State University, Dominguez Hills
8. `Sacramento Municipal Utilities Association, Sacramento Municipal Utility District` → Sacramento Municipal Utilities Association ; Sacramento Municipal Utility District
9. `Silver Creek Valley Country Club, Geologic Hazard Abatement District` → Silver Creek Valley Country Club ; Silver Creek Valley Geologic Hazard Abatement District
10. `Southland Partnership Corporation/P.O.W.E.R. Collaborative Network` → Southland Partnership Corporation ; P.O.W.E.R. Collaborative Network
11. `Sonoma County Herb Association and Organic Garden Club` → Sonoma County Herb Association ; Organic Garden Club
12. `Ti'at Society/Traditional Council of Pimu` → Ti'at Society ; Traditional Council of Pimu
13. `University of California San Diego, Veterans Affairs Medical Center` → University of California, San Diego ; VA San Diego Healthcare System
14. `Health Care Services and the Department of Social Services can more` → Department of Health Care Services ; Department of Social Services (strip trailing narrative "can more")
15. `Healthcare Association and the Alliance of Catholic Health` → California Healthcare Association ; Alliance of Catholic Health Care
16. `International Socioeconomic Society & Finxerunt Policy Institute` → International Socioeconomic Society ; Finxerunt Policy Institute
17. `Jago Bay Mutual Water Company and Home Owners' Association` → Jago Bay Mutual Water Company ; Jago Bay Home Owners' Association

---

## PART B — NEST: University / College sub-units (nest each as `chapter` under its parent institution)
Strip any embedded dean/officer/individual titles (fold as alt of the school, or drop the personal name). Truncated sub-unit names: nest under the parent as-is.

- American Indian Student Association, University of California, Irvine → UC Irvine
- American Indian Student Association, University of California, Los Angeles → UCLA
- American Indian Studies Center, University of California, Los (Angeles) → UCLA
- Asian Pacific Coalition, University of California Los Angeles → UCLA
- Athletic Department, University of California, Berkeley → UC Berkeley
- Disabled Students' Union, University of California, Berkeley → UC Berkeley
- Department of Ecology and Evolutionary Biology, University of Toronto, St. George → University of Toronto
- Digital Media Center, Pasadena City College → Pasadena City College
- Director, Center for Engaged Religious Pluralism, Saint Mary's College → Saint Mary's College
- Director, Police and Safety Services, Pasadena City College (both) → Pasadena City College
- Department of Public Service, Rio Hondo College → Rio Hondo College
- Training Center and Police Academy at Rio Hondo College → Rio Hondo College
- Rio Hondo College Disabled Students Programs and Services → Rio Hondo College
- Norco College Extended Opportunity Programs & Services → Norco College
- Santa Ana College, Extended Opportunity Programs & Services → Santa Ana College
- Santa Ana College, Health & Wellness Center → Santa Ana College
- Mission College Student Enrollment & Financial Services → Mission College
- Pasadena City College, Veterans Resource Center and Veteran Services → Pasadena City College
- Ohio State University, Bioproduct Innovation Center → Ohio State University
- Springfield College, School of Health & Human Services → Springfield College
- Indiana State University College of Nursing, Health and Human Services → Indiana State University
- Innocence Project, Benjamin W. Cardozo School of Law, Yeshiva University → Yeshiva University > Cardozo School of Law
- Institute for Social Research, University of Michigan → University of Michigan
- Institute of Environmental Health, University of Vienna → University of Vienna
- Life Span Institute, University of Kansas → University of Kansas
- University of Alabama at Birmingham, Department of Physical Medicine and Rehabilitation → UAB
- William and Cloy Codiga Resource Recovery Center, Stanford University → Stanford University
- University of Southern California, Race and Equity Center → USC
- University of San Diego, Center for Restorative Justice → University of San Diego
- University of San Diego, Department of Ethnic Studies → University of San Diego
- University of San Diego, Financial Aid Services → University of San Diego
- University of San Francisco, School of Law Dean and Assistant Dean for Academic Services → University of San Francisco
- University of La Verne, College of Law Dean and Assistant Dean, Center for Academic & Bar Readiness → University of La Verne
- Microbiology and Environmental Toxicology Department, University of California, Santa Cruz → UC Santa Cruz
- National Fuel Cell Research Center, University of California Irvine → UC Irvine
- Los Angeles Biomedical Research Institute, Harbor UCLA Medical Center → Harbor-UCLA Medical Center
- MIND Institute, UC Medical Center → UC Davis Medical Center
- Women's Center for Health, University of California Davis → UC Davis
- Women's Resources and Research Center, University of California, Davis → UC Davis
- Yosemite Community College District, District 4 → Yosemite Community College District
- (UC campus sub-units — all "University of California, <campus> …" — nest under the matching campus:)
  UC Hastings: Hastings College of the Law Students for a; Hastings College of Law Externships and Pro Bono Programs; Hastings College of Law Acting Chancellor & Dean...
  UC Irvine: Center for Virus Research; Dept of Pediatrics and Primary Care Medical Group; UCI Medical Center; School of Law Dean...; Environmental Law Society
  UCLA: UCLA Medical Center; Undergraduate Students Association Council; American Studies Center; Department of Social...; Health Services; School of Law Dean...; Seizure Disorder Center; Harbor-UCLA Medical Center; Neurology Dept Stroke Center; Department of Intercollegiate...
  UC Merced: Bright Success Center
  UC Riverside: Ethnic Studies Department; Center for Sustainable Suburban; College of Engineering - Center; Graduate Student Association; Student Association; Bourns College of Engineering
  UC San Diego: Dept of Pediatrics; Graduate Student Association; Student Chapter, Society of Health; Center for Public
  UCSF: Diabetes Center; AIDS Research Institute; Dept of Laboratory Medicine; Dept of Psychiatry; Institute for Health; Ob/Gyn & Reproductive Services; Student Chapter, Society of Health; Department of Aging
  UC Santa Cruz: American Studies Department; Marine Mammal Stranding Network; Student Union Assembly
  UC Davis: School of Medicine Dept of Pediatrics; Dept of Asian American Studies; Cancer Center; Dept of Anthropology Research Affiliate; Dept of Psychiatry Chair; Veterinary Blood Bank
  UC Berkeley: Center for Children's; City Planning + Public Health Master's Students Committee; College Writing Programs; College of Environmental Design Student(s) of Color; Berkeley Foundation (borderline—may stand alone); Water Center; (Associate) Vice Chancellor office; Planning Students Association
  University of California (systemwide): Center for Labor Studies; Center for Weight and Health; Graduate & Professional Council
  UCGHI: Women's Health, Gender, and Empowerment Center; Center for Gender and Health Justice

## PART C — NEST: City-government sub-units (nest each as `chapter` under `City of <X>`)
Strip embedded individual names (e.g. Rebecca Kaplan, George Chapjian) — leadership titles map to the body as alt.

- City of Glendale: Parks/Recreation/Community Services Dept; Fire Prevention Bureau; Planning Department (+ strip "Director George Chapjian")
- City of Hawthorne: Housing Authority
- City of Lafayette: Parks and Recreation Department
- City of Lathrop: Animal Services Department
- City of Lompoc: Beautification & Appearance Committee
- City of Long Beach: Department of Health and Human Services (+ "Health and Human Services"); 8th District; District 9
- City of Los Altos: Department of Public Works
- City of Los Angeles: Civil + Human Rights and Equity Department; Bicycle Advisory Committee; Board of Animal Regulation Commission; City Council Member 5th District; Department of Public Works; District 11; Environmental Affairs Department
- City of Monterey: Department of Public Works
- City of Napa: Community Resources Department
- City of Newport Beach: Fire Department
- City of Oakland: Bicyclist and Pedestrian Advisory Commission; Community and Economic Development Agency; Councilmember District 5; City Council Pro Tem (strip Rebecca Kaplan); Council President Pro Tempore (strip Rebecca Kaplan); Dept of Parks, Recreation and Youth Development; Fire Department; Youth Commission
- City of Orange: Fire Department
- City of Orinda: Parks and Recreation Department
- City of Pacifica: Parks, Beaches and Recreation Department
- City of Pasadena: Water and Power Department
- City of Pismo Beach: Conference & Visitors Bureau
- City of Richmond: District 4; District 6
- City of Rosemead: Parks and Recreation Department
- City of Roseville: Parks & Recreation / Roseville Sports Center
- City of Sacramento: Animal Care and Services; Mayor and Council; Animal Care Services Division; District 2; District 5; District 8; Law and Legislation Committee; Sanitation District
- City of San Bernardino: Parks and Recreation Commission; Economic Development Agency
- City of San Jose: Animal Care and Services; District 4; City Council District 9
- City of San Leandro: Recreation and Human Services
- City of San Marcos: Community Services (Director → alt)
- City of San Marino: Parks and Public Works Department
- City of Santa Ana: Fire Department
- City of Santa Cruz: Parks and Recreation Department (+ "...District" variant)
- City of Santa Maria: Recreation and Parks Department
- City of Scotts Valley: Department of Public (Works)
- City of Selma: Recreation and Community Services
- City of South El Monte: Community Services
- City of Stockton: District 3
- City of Tustin: Parks & Recreation Department
- City of Vacaville: Department of Public Works
- City of Vernon: Health and Environmental Control Department
- City of Visalia: Park and Recreation Commission
- City of Westminster: Community Services and Recreation Department
- Berkeley City Council, City Clerk Department → City of Berkeley

## PART D — NEST: County / Agency / Church / Union / Other sub-units (nest under the named parent)
- County of Alameda: Personnel, Administration and Legislation Committee; Board of Supervisors District 4
- County of Santa Cruz: Youth Resource Bank
- County of Ventura: Office of the District (Attorney)
- Los Angeles County: Department of Health and Human Services; Veterans Advisory Council; Community Perinatal Project
- San Mateo County Health: Health Services Division / Health Agency Division; AIDS Program (also route the "Client Services Coordinator…" individual to individuals CSV)
- State of California, Health and Human Services Agency: Department of Health
- Department of Consumer Affairs: Bureau of Automotive Repair
- Department of Defense: Department of the Army; Department of the Navy; Regional Environmental Coordinator District 9; Dept of the Navy Naval Command Control & Ocean Surveillance Center
- Department of Health and Human Services (federal): Indian Health Service
- Department of Transportation (Caltrans): District 11
- Fish and Game Commission: Opportunities Advisory Committee
- Service Employees International Union: California State Council (Employees International Union, California State Council)
- International Union: California State Council
- Laborers' International Union of North America: Committee of Public Employees
- Painters Union (IUPAT): Tradeshow and Sign Crafts Local Union 831
- Boys and Girls Club: Northern California Area Council
- Christian Methodist Episcopal Church: Ninth Episcopal District
- Italian Catholic Federation: Los Angeles District
- Evangelical Free Church of America: S.W. District Conference
- General Federation of Women's Clubs: Sutter District
- Peace and Freedom Party of California: San Diego County Central Committee
- California Republican Party: County Chairmen's Association
- League of Women Voters: Fremont-Newark-Union City local (two variant strings)
- LDS Social Services: California South Agency
- Ministry Services of the Daughters of Charity of St. Vincent De Paul: Saint Francis Center
- Church committees → parent church: Church and Society (First Presbyterian Church of San Anselmo); Church of St. Anne / Council of Stewards; Orinda Community Church / Creation and Justice Committee; St. Joseph the Worker Church / Social Justice Committee; Pasadena Jewish Temple and Center / Social Justice Committee; Lutheran Church of the Incarnation, Davis / Justice Committee
- The Unity Council: Children & Family Services
- Compass Community Services: Tenderloin Childcare Center
- Vista Hill Foundation: Sam and Rose Stein Education Center
- Felidae Foundation: Bay Area Puma Project
- The Stepping Stones Group: Community Autism Services (a Division of)
- The Foundation for Brain Science and Immunology: PANDAS Physicians Network (a Division of)
- Western Foundation of Vertebrate Zoology: Bird Museum and Research Center
- Western Burglar and Fire Alarm Association: Apprenticeship & Training Committee
- Urban & Environmental Policy Institute: Center for Food & Justice
- Dellums Institute for Social Justice: Policy Innovation + Justice Project
- IUCN > Species Survival Commission: Crocodile Specialist Group
- Sonoma State University: Project Censored
- Stanley Foundation: Research Institute
- CA STD and HIV Controllers Association: Executive Committee
- California Sexually Transmitted Disease... (see above)
- Palo Alto Unified School District: Division of Student Services
- Lancaster School District: Department of Special Programs
- Mount Diablo Unified School District: Foster Youth Services
- Monterey County Mayors' Association: City Selection Committee
- Civil Air Patrol, California Wing: Group 5 (NORCAL Group Commander)
- Humboldt Senior Resource Center: Adult Day Health & Alzheimer's Services
- Naval Medical Center: Department of Otolaryngology
- Palomar Medical Center: Professional Practice Committee
- Sequoia Hospital: Health & Wellness Services
- Mission Hospital: Mission Hospital Laguna Beach
- La Jolla Town Council: Parks & Beaches Committee
- North Westwood Neighborhood Council: Community Health & Homelessness Committee
- Business Law Section (State Bar): Partnership and LLC Committee
- Association of California Healthcare Districts: Alpha Fund (affiliated entity)
- California Small Business Development Center: North Coast Center; Los Angeles Regional Network
- Anderson Valley Community Services District: Fire Department
- W. C. Cox & Company: Research Bureau
- Merriwether & Williams Insurance Services: Testing Services and Inspection
- Panasonic Corporation: Energy Company
- Ministry of Natural Resources & Tourism (Tanzania): Tanzania Wildlife Management Authority
