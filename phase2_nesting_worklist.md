# Phase 2 — Stray-Nesting Worklist (fold standalone strays UP into big canonicals)

Source: MA-2 Phase-2 sweep (2026-07-03). 10 agents verified 1,869 candidate strays (standalone top-level canonicals whose name contains a big anchor's name as a phrase) against the live JSON, classifying each as **NEST-chapter** (local/regional/county affiliate or sub-unit → `chapter` under the anchor), **NEST-alt** (spelling/format variant of the anchor itself → `alternate_spelling`), or **SEPARATE** (distinct org / conjoined string / individual → do NOT nest).

**Only NEST verdicts are listed below.** Anything not listed = SEPARATE (skip). Tasks 38xx execute this file, one chunk per task. For every fold: verify against the live JSON, **merge-append** (never replace), preserve the stray's own children, and remove the emptied top-level canonical. All folds are within-crosswalk reorgs (no CSV moves) **except** the special-handling items below.

## Cross-cutting special handling (applies across all chunks)
- **Cerebral Palsy Association anchor** (chunk 6): its ~15 strays are all **United Cerebral Palsy (UCP)** units — do NOT nest under the generic "Cerebral Palsy Association"; instead consolidate them under a dedicated **`United Cerebral Palsy Associations`** canonical (create if absent; make each UCP unit a chapter). Handled by task, flagged.
- **CRLA Foundation** (chunks 4/6): `California Rural Legal Assistance Foundation` is a distinct legal entity → make it a `chapter` under `CALIFORNIA RURAL LEGAL ASSISTANCE` (supervisor Foundation→chapter rule). Its many conjoined "...Foundation, <other org>" strings are SEPARATE (route to conjoined CSV separately).
- **Renamed orgs → use `previously_known_as`, not a new nest**: `Consumer Watchdog` (formerly Foundation for Taxpayer and Consumer Rights); `UCSF Benioff Children's Hospital Oakland` (formerly Children's Hospital Oakland); `Prevail` (formerly Women's Center - Youth & Family Services); `California Environmental Voters` (formerly CLCV). Keep the current name as canonical, the former as `previously_known_as`.
- **Named individuals → route to `org_names_that_are_actually_individuals.csv`** (not nested): e.g. `Tom Panas, Member, West Contra Costa USD`; `John de Beck, San Diego USD`; `Michael Rubio, Kern County Board of Supervisors`; `Oakland USD Administrator Randolph Ward`; `Santa Clara COE Dave Cortese`; various trustees. (These surfaced as SEPARATE; list them for the individuals-CSV sweep.)
- **Conjoined multi-org strings → SEPARATE here**; they are the dominant SEPARATE cause and are candidates for a later conjoined-CSV split pass (out of scope for nesting).
- **Leadership-titled office strings → `alternate_spelling` of the office** (e.g. `Manohar Raju - San Francisco Public Defender`, `Executive Officer, California Board of Accountancy`, `Dean Logan ... Registrar-Recorder`).

---

## CHUNK 1  (task: PHASE2-NEST-1)

### Alliance for the Mentally Ill  → NEST-chapter (NAMI local chapters)
Alliance for the Mentally Ill, Mendocino Coast; San Gabriel Valley Alliance for the Mentally Ill; The South Bay Alliance for the Mentally Ill; San Francisco Alliance for the Mentally Ill; Alliance for the Mentally Ill of Long Beach; Alliance for the Mentally Ill of Santa Cruz County; Angels Camp Alliance for the Mentally Ill; Nevada County Alliance for the Mentally Ill; Alliance for the Mentally Ill, Yolo County; Coachella Valley Alliance for the Mentally Ill; The Alliance for the Mentally Ill of Metropolitan State Hospital; Alliance for the Mentally Ill, Clearlake Oaks; Alliance for the Mentally Ill San Bernardino Area; Alliance for the Mentally Ill Bakersfield; Alliance for the Mentally Ill, Kern County; Sacramento County Alliance for the Mentally Ill; East San Gabriel Valley Alliance for the Mentally Ill; Monterey County Alliance for the Mentally Ill; Rancho Cucamonga Alliance for the Mentally Ill; Alliance for the Mentally Ill Lake Co. CA; Alliance for the Mentally Ill Temecula; Family Alliance for the Mentally Ill, Southern Santa Barbara; Family Alliance for the Mentally Ill, Alameda County; Alliance for the Mentally Ill of Santa Clara County; Riverside Alliance for the Mentally Ill; San Bernardino Alliance for the Mentally Ill; Sonoma County Alliance for the Mentally Ill; Alliance for the Mentally Ill, San Rafael; Contra Costa County Alliance for the Mentally Ill; Palo Alto Alliance for the Mentally Ill; Ventura County Alliance for the Mentally Ill; Yuba-Sutter Alliance for the Mentally Ill; Alliance for the Mentally Ill of Inland; Alliance for the Mentally Ill, San Joaquin County; Alliance for the Mentally Ill, Marin; Alliance for the Mentally Ill of Shasta County; Humboldt Alliance for the Mentally Ill; Alliance for the Mentally Ill Oaks; Alliance for the Mentally Ill, Santa Clara; Mendocino County Alliance for the Mentally Ill; Temecula Valley Alliance for the Mentally Ill; Alliance for the Mentally Ill, Los Angeles County; Alliance for the Mentally Ill of Orange County; Alliance for the Mentally Ill of Contra Costa; Alliance for the Mentally Ill - Lake County; Alliance for the Mentally Ill Alameda County; Glendale Alliance for the Mentally Ill; San Diego Alliance for the Mentally Ill; San Luis Obispo County Alliance for the Mentally Ill; Alliance for the Mentally Ill of San Mateo County; Alliance for the Mentally Ill of Los Angeles; County Alliance for the Mentally Ill; Peopleos Alliance for the Mentally Ill, Mendocino Coast
(SEPARATE/conjoined — skip: Familias Unidas Alliance for the Mentally Ill; American Schizophrenic Association Alliance for the Mentally Ill; the 3 multi-county "...Contra Costa, Kern County, ..." strings.)

### International Union of Painters and Allied Trades  → NEST-chapter
International Union of Painters and Allied Trades, District; - Auto, Marine, and Specialty Painters; Painters, Tapers, Floorcoverers, & Glaziers; - Carpet, Linoleum, and Soft Tile; - Carpet, Linoleum, and Soft Tile Workers; 831 Trade Show; - Glaziers, Architectural Metal and Glass Workers; - Mixed Crafts; Local Union 376; /Finishing Trades; No. 16; - Carpet, Resilient Floor Covering, and Sign Workers
→ NEST-alt: International Union of Painters and Allied Trades, AFL-CIO

### California Nurses Association  → NEST-alt
California Nurses Association /National Nurses Organizing Committee (CNA/NNOC); The California Nurses Association (CAN); California Nurses Association and National Nurses United; California Nurses Association, National Nurses; California Nurses Association, California State; California Nurses Association, AFL-CIO; California Nurses Association/National Nurses Organizing Committee
→ NEST-chapter: California Nurses Association Hazard Management Services

### SAN FRANCISCO UNIFIED SCHOOL DISTRICT  → NEST-chapter (district units)
San Francisco Unified School District Community Advisory Committee For Special Education; San Francisco Unified School District Excelsior Child; San Francisco Unified School District, School Health; San Francisco Unified School District Advisory Committee For Special Education; San Francisco Unified School District Science Department
→ NEST-alt: San Francisco Unified School Districts

### NATIONAL ELECTRICAL CONTRACTORS ASSOCIATION  → NEST-chapter (CA/regional chapters)
California Chapters of the National Electrical Contractors Associations; National Electrical Contractors Association, Los Angeles County; National Electrical Contractors Association, Santa Clara Valley; Kern County Chapter of the National Electrical Contractors Association; National Electrical Contractors Association-California Chapter; National Electrical Contractors Association, Santa Clara

### Sacramento City Unified School District  → NEST-chapter
Sacramento City Unified School District's Child Development; Sacramento City Unified School District, Elder Creek Elementary; Sacramento City Unified School District Board of Education; Sacramento City Unified School District, Peter Burnett; Sacramento City Unified School District Special Education; Sacramento City Unified School District, Woodbine Elementary

### Smaller anchors (chunk 1)
- California Building Industry Association → NEST-chapter: ...PROFESSIONAL WOMEN IN BUILDING COUNCIL; NEST-alt: ...Home; ...San (also see CHUNK-BIA task 3822 — coordinate)
- Association of County Veterans Service Officers → NEST-chapter: County of Orange Association of County Veterans Service Officers; NEST-alt: California Association of County Veterans Service Officers, Inc. (favors removing sunset)
- Glendale Community College → NEST-chapter: Associated Student Body (ASB) of Glendale Community College; Professional Development Center of Glendale Community College; Glendale Community College - Student Financial Aid; Associated Students of Glendale Community College; NEST-alt: Glendale Community College District
- California Association of Health Facilities → NEST-alt: The California Association of Health Facilities (CAHF), as well; NEST-chapter: California Association of Health Facilities Developmental Services Conference
- CALIFORNIA FINANCIAL SERVICES ASSOCIATION → NEST-chapter: California Financial Services Association, Independent Section; Law Committee of the California Financial Services Association
- CALIFORNIA STATE ASSOCIATION OF PUBLIC → NEST-alt: California State Association of Public Administrators, Public Guardian, and Public Conservators; California State Association of Public Administrators, Public Guardians and Public
- IN DEFENSE OF ANIMALS → NEST-chapter: South Bay in Defense of Animals; Silicon Valley in Defense of Animals
- Reckitt Benckiser → NEST-chapter: Reckitt Benckiser Pharmaceuticals; Reckitt Benckiser North America
- Hewlett-Packard → NEST-chapter: Hewlett Packard Components Group
- Marine Corps Installations West → NEST-alt: United States Marine Corps, Marine Corps Installations West; Marine Corps Installations West-Marine Corps Base
- United Union of Roofers, Waterproofers and Allied Workers → NEST-chapter: ...Local Union 36
- San Jose Unified School District → NEST-chapter: Washington Elementary School, San Jose Unified School District
- Riverside County Office of Education → NEST-chapter: ...Division of Children and Family Services
- Glendale Memorial Hospital and Health Center → NEST-chapter: Volunteer Service Department of Glendale Memorial Hospital and Health Center
- Los Angeles County Juvenile Court → NEST-chapter: ...Dependency Division (strip judge name)
- Mission Economic Development Agency → NEST-chapter: Fondo Adelante, Mission Economic Development Agency
- East Palo Alto Council of Tenants → NEST-alt: EPACT East Palo Alto Council of Tenants
- California Food Policy Advocates → NEST-chapter: California Food Policy Advocates, San Francisco
- Association of Mexican American Educators → NEST-alt: ...Latino Educators
- California College and University Police Chiefs Association → NEST-alt: ...Chief
- American Heart Association/American Stroke Association → NEST-alt: (AHA/ASA) [note: AHA merge handled in task 3820]
- California Association of School Business Officials → NEST-alt: Programs, California Association of School Business Officials

---

## CHUNK 2  (task: PHASE2-NEST-2)

### Association for Retarded Citizens  → NEST-chapter (ARC local chapters)
Orange County Association for Retarded Citizens; Association for Retarded Citizens-Long Beach; Association for Retarded Citizens-Mid Cities; Indian Wells Valley Association for Retarded Citizens; Association for Retarded Citizens, San Francisco; Association for Retarded Citizens-Alameda County; Ontario/Pomona Association for Retarded Citizens; Association for Retarded Citizens Inc. (Long Beach); Stanislaus County Association for Retarded Citizens; Association for Retarded Citizens Training Center; Association for Retarded Citizens, Long; Association for Retarded Citizens, San

### Dignity Health  → NEST-chapter (member hospitals)
Mercy Medical Center - Merced (Dignity Health); Mercy San Juan Medical Center (Dignity Health); Inland Empire (Dignity Health); Saint Francis Memorial Hospital (Dignity Health); St. Joseph's Medical Center, Stockton (Dignity Health); Marian Regional Medical Center (Dignity Health); Sequoia Hospital (Dignity Health)

### Chula Vista  → NEST-chapter (true city departments only)
City of Chula Vista Fire Department; Chula Vista Fire Department; Chula Vista Animal Care Center; Chula Vista Animal Services; Chula Vista Police Chief; City of Chula Vista Veterans Advisory Commission; Healthy Chula Vista Advisory Commission; Chula Vista City Councilwoman, Pamela Bensoussan
(Everything else under "Chula Vista" = SEPARATE: hospitals, schools, businesses, nonprofits merely located there.)

### Cuesta College  → NEST-chapter
Cuesta College CalWORKs; Cuesta College - Financial Aid; Cuesta College Health Services; Cuesta College Business & Entrepreneurship Center; Business & Entrepreneurship Center Director, Cuesta College

### Smaller anchors (chunk 2)
- California State Association of Counties → NEST-alt: The California State Association of Counties (CSAC); California Chapter California State Association of Counties
- Consumer Attorneys of California → NEST-alt: The Consumer Attorneys of California (CAOC)
- Trust for Public Land → NEST-alt: Member Trust for Public Land; NEST-chapter: California Trust for Public Land Action Fund; Central Valley Program Director Trust for Public Land
- California Military Department → NEST-alt: Major General Paul D. Monroe, Jr., The Adjutant General...; NEST-chapter: State of California Military Department, State Military Reserve; State of California Military Department Headquarters, Camp San Luis Obispo
- Congress of California Seniors → NEST-chapter: Congress of California Seniors Council
- Metropolitan Transportation Commission → NEST-alt: Bay Area Metropolitan Transportation Commission
- South Coast Air Quality Management District → NEST-chapter: ...Legislative Committee
- American GI Forum of California → NEST-chapter: Clearlake Oaks American GI Forum of California; American GI Forum of California Veterans Outreach Program, Santa
- Bricklayers and Allied Craftworkers → NEST-alt: International Union of Bricklayers and Allied Craftworkers; Union of Bricklayers and Allied Craftworkers
- California Church Impact → NEST-alt: ...Sacramento; ...Public Policy Coordinator
- California Right to Life Committee → NEST-alt: California Right to Life Committee, Inc. (CRLC)
- Coalition to Abolish Slavery and Trafficking → NEST-chapter: ...Survivor Advisory Council; NEST-alt: ...LA
- Crossroads of the West → NEST-alt: B & L Productions Dba Crossroads Of The West Gun Shows; CROSSROADS OF THE WEST GUN SHOWS
- Gay-Straight Alliance Network → NEST-chapter: Bay Area Gay-Straight Alliance Network
- Agua Caliente Band of Cahuilla Indians → NEST-alt: Tribal Council of the Agua Caliente Band of Cahuilla Indians
- California Association of School Psychologists → NEST-chapter: Central Valley Affiliate of the California Association of School Psychologists
- California Foundation for Independent Living Centers → NEST-alt: ...Board of Directors
- California Off-road Vehicle Association → NEST-alt: California Off Road Vehicle Associations, Inc
- Center for Energy Efficiency and Renewable Technologies → NEST-alt: North San Juan CEERT - ...
- Fair Political Practices Commission → NEST-alt: Ca Fair Political Practices Commission
- Independent Administrators Association → NEST-alt: Independent Administrators Association of California
- Los Angeles Regional Reentry Partnership → NEST-alt: County of Los Angeles Regional Reentry Partnership
- Northern California Power Agency → NEST-alt: Northern California Power Agency, Southern
- Service Employees International Union → NEST-chapter: Legislative Council, Service Employees International Union
- Dignity Health / American Institute of Architects / California Transit Assn / Public Counsel: all-SEPARATE (conjoined) — skip
- American Planning Association: all-SEPARATE — skip
- California Church Impact, Campaign for California Families → NEST-alt: The Campaign for California Families (CCF); NEST-chapter: Campaign for California Families Committee on Moral
- Community Alliance with Family Farmers → NEST-chapter: ...Agricultural-Natural Resources Trust
- Little Hoover Commission → NEST-alt: The Milton Marks "Little Hoover" Commission on California
- National Lawyers Guild → NEST-chapter: National Lawyers' Guild of the San Francisco Bay Area
- Organization of Chinese Americans → NEST-chapter: San Francisco Organization of Chinese Americans
- Tarzana Treatment Centers → NEST-alt: Tarzana Treatment Centers Coalition (VERIFY first)
- Fair Housing Council of Riverside, Fresno Interdenominational Refugee Ministries → NEST-alt: IRM, Inc (Fresno Interdenominational Refugee Ministries)

---

## CHUNK 3  (task: PHASE2-NEST-3)

### SOCIETY FOR HUMAN RESOURCE MANAGEMENT (SHRM)  → NEST-chapter
Southern California Wine Country SHRM; SHRM - Central California; SHRM - Wine Country; SHRM - Kern County; SHRM Northern California; SHRM in California; SHRM - San Diego; SHRM - Northstate

### California Community Colleges  → NEST-chapter (system governance/offices/programs only)
California Community Colleges, Vice Chancellor's Office; Board of Governor(s) of the California Community Colleges [+ "...Chancellor's", "Governors of the..."]; California Community Colleges Chancellor's Office, Allied Health Programs; ...Employer Engagement for Agriculture, Water & Environmental Programs; ...Agriculture & Natural Resources; Biotechnology Initiative of the...; ...Economic Development Network; ...Commission on Athletics; ...Classified Senate; ...LGBTQ+ Advisory Committee
→ Student Senate regions (regroup under a `Student Senate for California Community Colleges` node): Region II; Region III; Regions 4 and 7; Regions IV and VIII; Region VII; Region VIII; Region IX; Region X; Past President, Student Senate...
(All the distinct associations/foundations/leagues/caucuses = SEPARATE — skip.)

### Underground Scholars Initiative  → NEST-chapter (UC campus chapters)
...University of California, San Diego; ...UC Irvine; ...Riverside; ...University of California Davis; ...University of California Los Angeles

### Natural Resources Defense Council  → NEST-alt/chapter
NEST-alt: Incorporated Natural Resources Defense Council; Natural Resources Defense Council, when RDAs. NEST-chapter: Climate and Energy Natural Resources Defense Council; Santa Cruz NRDC

### Smaller anchors (chunk 3)
- American Insurance Association → NEST-chapter: ...California Council; NEST-alt: The American Insurance Association (AIA)
- California Commission on Aging → NEST-alt: State of California Commission on Aging
- California Society of Certified Public Accountants → NEST-chapter: Group Insurance Trust of the...
- Southern California Association of Governments → NEST-chapter: ...Transportation and Communications Committee; Regional Council of the...
- California Motor Car Dealers Association → NEST-chapter: Northern California Motor Car Dealers Association; NEST-alt: ...Personal Insurance
- Consumer Federation of California → NEST-alt: Research Group: Consumer Federation of California; NEST-chapter: ...San Mateo
- Deposition Reporters Association → NEST-alt: The California Deposition Reporters Association (CDRA); Options Deposition Reporters Association
- Behavioral Health Directors Association → NEST-alt: The California Behavioral Health Directors' Association (CBHDA)
- California Attractions And Parks Association → NEST-alt: CAPA (California Attractions and Parks Association)
- California Independent Bankers → NEST-alt: California Independent Bankers
- California Orthotics and Prosthetics Association → NEST-chapter: Southern California Orthotics and Prosthetics Association (VERIFY)
- Community Action Board of Santa Cruz County → NEST-alt: City of Community Action Board of Santa Cruz County
- Fair Share Network → NEST-alt: CA Fair Share Network (Santa Cruz)
- Greater California Livery Association → NEST-alt: The Greater California Livery Association (GCLA)
- Los Angeles Registrar-Recorder/County Clerk → NEST-alt: ...Dean Logan
- Morongo Band of Mission Indian → NEST-alt: Morongo Band of Mission Indians, Tribal Council
- Northridge Hospital Medical Center → NEST-chapter: Center for Reproductive Medicine, ...
- Procter & Gamble → NEST-alt: The Procter & Gamble Company (P&G)
- Southern California Indian Center → NEST-chapter: ...Education Component, Parent Advisory Committee
- Utility Consumers' Action Network → NEST-alt: Ucan - Utility Consumers' Action Network
- Underground Scholars / American Insurance / California Alliance for Consumer Protection → NEST-alt: ...As Introduced; NEST-chapter: Azusa California Alliance for Consumer Protection
- National Center for Youth Law → NEST-alt: City of National Center for Youth Law
- Deposition Reporters / Behavioral Health / Air-conditioning & Refrigeration Contractors → NEST-alt: Air Conditioning & Refrigeration Contractors Association

---

## CHUNK 4  (task: PHASE2-NEST-4)

### Cement Masons  → NEST-chapter / NEST-alt (OPCMIA)
NEST-chapter: District Council of Plasterers and Cement Masons; ...of Southern California; Cement Masons Local No. 600; Northern California Cement Masons; Plasterers' and Cement Masons' Local Union No. 300; Direct Council...of Southern California; Operative Plasterers and Cement Masons' Beach Cities; Cement Masons Local Union No. 500; District Council...of Northern California; State Conference of Plasterers & Cement Masons.
NEST-alt: International Association of Operative Plasterers and Cement Masons; Plasterers and Cement Masons' International Association; Cement Masons Union; Plasterers and Cement Masons; Operative Plasterers and Cement Masons; Operative Plasterer's, Cement Masons' and Shop Hands

### American College of Obstetricians and Gynecologists  → NEST-chapter (District IX = CA)
...Dist IX, CA; The American College...District IX (ACOG); ...Region; ...IX/CA; ...Region IV; ...District IX (ACOG-IX); The...District IV; ...District 1X

### Student Senate for California Community Colleges  → NEST-chapter (regions)
Regions IV and VIII; Region II; Region VII; Region III; Region VIII; Regions 4 and 7; Region IX; Region X  (→ NEST-alt: Past President, Student Senate...)

### Iron Workers  → NEST-chapter/alt
NEST-chapter: District Council of Iron Workers of the State of California and the Vicinity. NEST-alt: Reinforcing Iron Workers; Structural, Ornamental, Iron Workers, Riggers, Heavy Machinery; International Association of Iron Workers; Bridge, Structural, Ornamental and Reinforcing Iron Workers; Ornamental Iron Workers

### United Steelworkers  → NEST-chapter/alt
NEST-alt: United Steelworkers, USW; United Steelworkers Union. NEST-chapter: Glass, Molders, Pottery, Plastics and Allied Workers Council of the USW; United Steelworkers Wood Council; United Steelworkers Inland Empire Legislative and Education Committee; United Steelworkers, Glass Molders, Pottery, Plastics and Allied Workers International Council

### California Democratic Party  → NEST-chapter (additional caucuses)
Rural Caucus of the California Democratic Party; California Democratic Party-Environmental Caucus; Disability Caucus of the California Democratic Party; Irish American Caucus, California Democratic Party

### Showing Up for Racial Justice  → NEST-chapter
SURJ Contra Costa County; Vashon-Maury Showing Up for Racial Justice; SURJ Bay Area; Surj Marin

### Animal Legal Defense Fund  → NEST-chapter
UC Davis Student Animal Legal Defense Fund; Hastings Student Animal Legal Defense Fund; Animal Legal Defense Fund (San Francisco)

### National Organization for the Reform of Marijuana Laws  → NEST-chapter
California NORML (Cal); NORML Women's Alliance; San Diego Chapter - NORML

### Smaller anchors (chunk 4)
- California Nurses Association: see chunk 1
- Communication Workers of America → NEST-alt: T. Santora, President, ...; NEST-chapter: Alphabet Workers Union - Communication Workers of America
- California Municipal Utilities Association → NEST-alt: ...East
- Council of Community Housing Organizations → NEST-alt: ...San Francisco; San Francisco Council of Community Housing Organizations
- Stop the Spray → NEST-chapter: Cities to Stop the Spray (Alameda); Coalition of California Cities to Stop the Spray (Alameda); NEST-alt: Coalition for California Cities to Stop the Spray
- California Commission on the Status of Women → NEST-alt: ...and Girls
- Campaign for California Families → NEST-alt: The Campaign for California Families (CCF)
- Community Alliance with Family Farmers → NEST-chapter: ...Agricultural-Natural Resources Trust
- Dow Chemical Company → NEST-alt: Dow Chemical Company and Its Affiliate, Dow Agrosciences
- Guam Communications Network → NEST-alt: Guam Communications Network-Lola Sablan-Santos
- Trans Union → NEST-alt: Trans Union Credit Reporting Agency; Trans Union Credit Corporation
- Burlington Northern Santa Fe Railway Company → NEST-alt: BNSF (Burlington Northern Santa Fe) Railway Company
- California Pharmacists Association → NEST-chapter: Long Term Care Management Council of the...
- Central City Association of Los Angeles → NEST-alt: CALIFORNIA CENTRAL CITY ASSOCIATION OF LOS ANGELES
- Consejo de Federaciones Mexicanas en Norteamérica → NEST-alt: COFEM - ...
- Ella Baker Center for Human Rights → NEST-chapter: Green-Collar Jobs Campaign of the Ella Baker Center...
- Grocery Manufacturers Association → NEST-chapter: California Grocery Manufacturers' Association
- Latin Business Association → NEST-chapter: NORTHERN CALIFORNIA LATIN BUSINESS ASSOCIATION
- Northrop Grumman → NEST-chapter: Northrop Grumman Aerospace Systems
- Professional and Technical Engineers → NEST-alt: Federation of Professional and Technical Engineers [IFPTE — see task 3818]
- Southern California Public Power Authority → NEST-alt: Board of Directors of the...
- The Boeing Company → NEST-alt: D.S.L - The Boeing Company

---

## CHUNK 5  (task: PHASE2-NEST-5)

### Brady Campaign to Prevent Gun Violence  → NEST-chapter (all "... Chapter of the Brady Campaign...")
San Joaquin Valley; Santa Barbara County; San Fernando Valley; Antelope Valley; Oakland/Alameda County; San Diego County; San Francisco; Marin County; Santa Clara County; San Joaquin County; Yolo County; Sacramento; Sonoma County; Santa Cruz; Oakland/Alameda; Orange County; Pomona Valley; Long Beach; Santa Cruz County; Ventura County; South Bay LA; Contra Costa County; Santa Barbara; Los Angeles County; Solano County; Nevada County; Napa; Sacramento Valley; San Mateo County; Los Angeles; Cleveland School Remembers-brady Campaign ... Chapter
→ NEST-alt: CA CHAPTERS OF THE BRADY CAMPAIGN...; Chapters of the Brady Campaign...

### American Cancer Society  → NEST-chapter
American Cancer Society (Santa Cruz Unit); (Marin Unit-West Bay); (Santa Cruz County); Border Sierra Region; - California Office; California Division; American Cancer Society / Relay for Life; American Cancer Society Cancer Action Network Ins (ACS CAN)

### Sheet Metal Workers'  → NEST-chapter/alt
NEST-chapter: California Council of Sheet Metal Workers; Council of Sheet Metal Workers; States Council of Sheet Metal Workers; California State Association of Sheet Metal Workers; State Association of Sheet Metal Workers; Sheet Metal Workers' International Association Local Union; ...Local Union 104; Sheet Metal Workers' Local Union No. 104 (Smart); Sheet Metal Workers 104; Northern California Sheet Metal Workers' Local Union No. 104; Local Union 105 Sheet Metal Workers; Sheet Metal Workers' International Association Local Union 105; Sheet Metal Workers' Local Union 104 and 105.
NEST-alt: Association of Sheet Metal Workers; Sheet Metal Workers (SMWIA); Sheet Metal Workers International Union; States Sheet Metal Workers; State Sheet Metal Workers

### West Contra Costa Unified School District  → NEST-chapter
...Bilingual and ELD Services; ...Cameron School; ...SELPA; ...Special Education Department; ...School Board

### California Department of Education  → NEST-chapter
...Governmental Affairs; ...DataQuest; ...Career Development; ...California School for the Deaf

### United Teachers Los Angeles  → NEST-chapter
...Retired; ...Adult and Occupational Education Committee; United Teachers Los Angeles-Pace

### Utility Workers Union of America  → NEST-chapter
...Region 5; UTILITY WORKERS UNION OF AMERICA, LOCALS 132, 486, AND 522

### Smaller anchors (chunk 5)
- Auto Dismantlers Association → NEST-alt: California Auto Dismantlers Association; NEST-chapter: Valley Auto Dismantlers Association; Inland Auto Dismantlers Association; Auto Dismantlers Association of Southern California
- Enterprise Rent-a-Car → NEST-chapter: ...Company of San Francisco; ...of Sacramento; NEST-alt: Enterprise Holdings, Inc; Enterprise Rent-A-Car/Leasing
- Association of Bay Area Governments → NEST-alt: ...Authority
- Franchise Tax Board → NEST-chapter: State of California, Franchise Tax Board, Legislative Services Bureau; NEST-alt: State of California Franchise Tax Board
- California Primary Care Association → NEST-chapter: California Health+Advocates, Subsidiary of the...; California Primary Care Association Advocates; NEST-alt: The California Primary Care Association (CPCA)
- Emergency Nurses Association → NEST-chapter: California State Council of the...; California Emergency Nurses Association - California State; San Diego-Imperial Chapter...
- National Rifle Association → NEST-chapter: NRA Members' Council of the San Fernando (Note: the two "Note:.../this bill does" narrative strings → narrative CSV)
- California Association for the Education of Young Children → NEST-chapter: Southern California Association...; Inland Empire Chapter of...
- California Department of Transportation → NEST-alt: California Department of Transportation (Caltrans); State of California Department of Transportation
- Central Basin Municipal Water District → NEST-alt: ...(Division V residents); ...(Board of Directors and Division V residents)
- Community Associations Institute → NEST-chapter: Committee of the...
- Los Angeles World Airports → NEST-alt: ...- City of Los Angeles; ...Authority
- Apartment Association of Greater Los Angeles → NEST-alt: APARTMENT ASSOCIATION OF GREATER LOS ANGELES, SANTA BARBARA, AND SAN DIEGO
- California Board of Accountancy → NEST-alt: Executive Officer, California Board of Accountancy
- California Dispute Resolution Council → NEST-chapter: ...Community Board Program
- California Pro-Life Council → NEST-chapter: Tulare County Chapter, ...
- Cerritos College → NEST-chapter: Cerritos College Leaders Involved in Creating Change Program
- Endangered Habitats League → NEST-alt: Endangered Habitats League (also)
- Grossmont Union High School District → NEST-chapter: ...Athletic Conference
- Instituto de Educacion Popular del Sur de California → NEST-alt: IDEPSCA - ...
- National Electrical Manufacturers Association → NEST-chapter: Dry Cell Battery Section of the...
- Santa Ynez Band of Chumash Indians → NEST-alt: ...Chumash Casino
- Southern California Regional Rail Authority → NEST-alt: Metrolink-...
- Traditional Values Coalition → NEST-chapter: Contra Costa County Traditional Values Coalition
- Warehouse, Processing and Distribution Workers' Union → NEST-alt: International Warehouse, Processing and Distribution Workers' Union
- Legal Defense and Educational Fund → NEST-alt: American Legal Defense and Educational Fund [MALDEF — see task 3820]
- California Alliance for Retired Americans, Southern California Gas Company → NEST-alt: Balance, Southern California Gas Company (dirty; low value)

---

## CHUNK 6  (task: PHASE2-NEST-6)

### CALIFORNIA RURAL LEGAL ASSISTANCE  → NEST-chapter
California Rural Legal Assistance - Marysville; California Rural Legal Assistance (CRLA) Foundation; California Rural Legal Assistance Foundation - ND LTRS; California Rural Legal Assistance Fund  (Foundation → chapter; see cross-cutting rule)

### Cerebral Palsy Association strays → CONSOLIDATE UNDER new/other `United Cerebral Palsy Associations` canonical (NOT the anchor)
All ~15 "United Cerebral Palsy" units (Orange County, Central California, San Diego, Santa Clara/San Mateo, Inland Empire, Stanislaus, Fresno/Tulare, of California, etc.).

### AMERICAN FRIENDS SERVICE COMMITTEE  → NEST-chapter (programs/regional offices)
...Rural Economic Alternatives; ...San; ...Los Angeles; ...Proyecto Campesino; ...U.S.-Mexico Border Program; ...'s US-Mexico Border Project; ...Pacific Mountain; ...Pacific Mountain Region; ...Los Angeles, Roots for Peace Program

### American Heart Association  → NEST-chapter (see task 3820 for the AHA/ASA merge)
...California and Greater Los Angeles; ...-California and Greater; ...Western States Affiliate; ...-California and Greater Los Angeles Affiliates; ...California & Greater L.A; ...San Mateo County Division

### Lockheed Martin  → NEST-chapter (divisions)
Lockheed Martin Space Systems; ...Aircraft and Logistics; ...Missiles and Space; ...Skunk Works; ...Aeronautics Company; NEST-alt: Lockheed, Martin Missiles

### Women's Foundation of California  → NEST-chapter (programs/institutes)
Women's Foundation of California, Women's Policy Institute Trauma Justice Team; ...Criminal Justice Team; The Dr. Beatriz Maria Solis Policy Institute - Women's Foundation of California

### Smaller anchors (chunk 6)
- California Association of Clerks and Election Officials → NEST-chapter: ...Elections Legislative Committee; NEST-alt: (CACEO) Duf Sundheim; ...Clerk of the Board; ...(Secs. 4); ...Robert W. Naylor; ...Duf Sundheim
- Nature Conservancy → NEST-chapter: Nature Conservancy, Sacramento
- California Farm Bureau Federation → NEST-chapter: California Farm Bureau Federation, Trinity County
- Jewish Public Affairs Committee → NEST-chapter: Contra Costa Counties Jewish Public Affairs Committee of California; NEST-alt: JPAC-Jewish Public Affairs Committee of California
- California Department of Justice → NEST-alt: State of California Department of Justice (Attorney General); State of California, Department of Justice, Office of the Attorney General; California Department of Justice, Office of the Attorney General
- Independent Energy Producers → NEST-alt: Limited Partnership Independent Energy Producers Assn; Independent Energy Producers Assn (IEP); California Independent Energy Producers Association
- California Probation Parole and Correctional Association → NEST-chapter: ...Alameda County
- General Motors → NEST-chapter: General Motors Investment Management Corporation; General Motors Acceptance Corporation (GMAC)
- Association of California Cities - Orange County → NEST-alt: (ACC-OC)
- California Association of County Treasurers and Tax Collectors → NEST-chapter: ...County of Los Angeles; ...Orange County Treasurer-Tax Collector
- California Environmental Voters → NEST-alt: California Environmental Voters (formerly CLCV) [use previously_known_as]
- Chinese for Affirmative Action → NEST-alt: ...San Francisco/Sacramento
- East Bay Community Law Center → NEST-chapter: Clean Slate Practice of the...; Clean State Reentry Legal Servicers Practice, ...
- Loyola Marymount University → NEST-chapter: Center for Equity for English Learners, ...; Center for Ignatian Spirituality, ...
- Planning and Conservation League → NEST-alt: National Planning and Conservation League; Planning and Conservation League 6/18
- Asian American Legal Center → NEST-alt: Asian American Legal Center of Southern California
- California Association for Micro Enterprise Opportunity → NEST-alt: CAMEO Network (...)
- California Certified Organic Farmers → NEST-chapter: ...Pacific Southwest Chapter
- California Dump Truck Owners Association → NEST-chapter: ...Coachella Valley
- Consumer Protection Policy Center → NEST-alt: University of California, San Diego Consumer Protection Policy Center
- Health Care For All → NEST-alt: Health Care for All Californians (VERIFY)
- Inter-Agency Council on Child Abuse and Neglect → NEST-alt: Los Angeles County Inter-Agency Council on Child Abuse and Neglect [see task 3820 ICAN]
- Legion of Valor → NEST-chapter: California Chapter of the Legion of Valor
- Lutheran Office of Public Policy → NEST-alt: Lutheran Office of Public Policy in California
- National Federation of Filipino American Associations → NEST-chapter: ...Region 8, Northern
- Transportation Agency for Monterey County → NEST-alt: City of Transportation Agency for Monterey County

---

## CHUNK 7  (task: PHASE2-NEST-7)

### United Association  → NEST-chapter (plumber/pipefitter locals) / NEST-alt (UA full-name variants)
NEST-chapter: United Association Locals 78, 250; United Association, District Council No. 36; United Association Plumber's Local Union 78; United Association Local Union 761; United Association, District Council No. 16; United Association of Plumbers, Pipefitters and Refrigeration Fitters Local No. 246; United Association Local Union 582; United Association of Plumbers and Steamfitters Local Union 484; United Association Local Union 159; United Association of Local Union 159 - Martinez; United Association Local Union 228 Plumbers - Pipefitters; United Association of Plumbers, Pipefitters, and Sprinkler Fitters, Local; United Association, District Council No. 51; United Association of Plumbers and Steamfitters Local Union 230; United Association Local Union 114; United Association of Plumbers Pipe and Refrigerator Fitters Local No. 246.
NEST-alt: the ~10 "United Association of Journeymen & Apprentices of the Plumbing & Pipe..." variants + "United Association of Plumbers, Pipe Fitters and Sprinkler" + "...Union of Plumbers, Fitters, Welders, and Service Techs"

### Alzheimer's Association  → NEST-chapter
Alzheimer's Association California Council; ...Northern California; ...Butte, Glenn and Tehama Counties; ...State Policy Office; ...San Diego; Policy Council of the...; ...Central Coast; ...of Los Angeles; (Monterey County Chapter); ...of Los Angeles, Riverside and San Bernardino Counties; (Monterey); ...Ventura County; ...Greater Sacramento; Los Angeles Alzheimer's Association (Medical & Scientific Advisory Board); California Chapter of the...; Alzheimer's Association-California Chapters. NEST-alt: The Alzheimer's Association; National Alzheimer's Association

### Concerned Women for America  → NEST-chapter
...of California (San Diego, Riverside and Imperial Counties); ...Legislative Action Committee; ...for California; Concerned Women for American (CWA) of California; ...of California, Carlsbad; ...of California, San Diego and Imperial Counties; ...of California - San Diego/Imperial; ...-San Diego and Imperial Counties; ...(CWA) of California, San Diego & Imperial Counties; ...of Central California; ...of California (CWA). NEST-alt: Concerned Women for American; Concerned Women for America of Children

### Governor's Office  → NEST-chapter
Governor's Office of Child Development and Planning; ...Business and Economic Development; ...for Education; ...Homeland Security; ...Criminal Justice Planning; ...Emergency Services; ...Policy and Research; ...Assessment Management; ...on Service and Volunteerism; ...State Assets Management; California Governor's Office of Emergency Services; ...Legislation

### California Healthcare Association  → NEST-chapter/alt
NEST-alt: The California Healthcare Association (CHA); California Healthcare Association (Formerly the California). NEST-chapter: Physicians Groups Council, ...; ...Center for Behavioral; ...California Physician Groups; ...Physician Groups Council; ...Rural Healthcare Center

### JEWISH COMMUNITY RELATIONS COMMITTEE  → NEST-chapter
...of Los Angeles Jewish Federation; ...of Greater Los Angeles; ...The Jewish Federation; ...of the Los Angeles Jewish; The Jewish Federation of Greater Los Angeles, ...JCRC; ...of San Francisco Jewish Federation

### International Brotherhood of Electrical Workers  → NEST-chapter/alt
NEST-chapter: IBEW 465; IBEW 9th District; California-Nevada Conference IBEW. NEST-alt: IBEW, AFL-CIO; IBEW (IBEW) Locals

### Boy Scouts of America  → NEST-chapter (councils/troops)  [see CHUNK 8 list — this family is in chunk 8]

### Smaller anchors (chunk 7)
- Laborers' International Union of North America → NEST-chapter: California State Council of Laborers - LIUNA
- AIDS Project Los Angeles → NEST-chapter: ...Health and Wellness; ...(APLA) Health
- California Chiropractic Association → NEST-alt: The California Chiropractic Association; NEST-chapter: ...Southern California
- Institute of Scrap Recycling Industries → NEST-chapter: California Chapter, ...; California Chapter of the ...(ISRI); NEST-alt: ISRI- ... [also task 3821]
- San Diego Unified School District → NEST-chapter: ...Special Education Local (SELPA)
- Battered Women's Alternatives → NEST-alt: Domestic Violence Battered Women's Alternatives (BWA); NEST-chapter: ...Legal Advocacy Program (Concord)
- California Building Officials → NEST-alt: ...Roofing; The California Building Officials Board of Directors
- California Hospital Association → NEST-alt: The California Hospital Association (CHA)
- California Union of Safety Employees → NEST-alt: Bargaining Unit 7 (...); NEST-chapter: ...Code Enforcement Association
- Coalition for Adequate School Housing → NEST-alt: California's Coalition for Adequate School Housing (CASH)
- Correctional Peace Officers Association → NEST-chapter: CMF Chapter of the CCPOA (strip "Numerous MTAs") [CCPOA merge in task 3818]
- Association of Certified Family Law Specialists → NEST-alt: California Association of Certified Family Law Specialists (ACFLS)
- California Association of Persons with Handicaps → NEST-chapter: Beach-Wood Chapter, ...
- California Society of Anesthesiologists → NEST-alt: ...Inc. (CSA)
- Fresno Interdenominational Refugee Ministries → NEST-alt: IRM, Inc (...)
- Heat and Frost Insulators and Allied Workers → NEST-alt: Association of Heat and Frost Insulators and Allied Workers [see task 3818]
- International Brotherhood of Teamsters → NEST-alt: ...Joint
- Maternal, Child, and Adolescent Health Action → NEST-alt: California Maternal, Child and Adolescent Health Action
- National Health Law Program → NEST-chapter: ...Los Angeles
- San Bernardino Public Employees → NEST-alt: San Bernardino Public Employees Union
- Southwest Healthcare System → NEST-chapter: ...Murrieta & Wildomar
- SPCA Los Angeles → NEST-alt: Animal Advocates SPCA - Los Angeles (VERIFY)
- Triple A Council of California → NEST-alt: TAAC Triple-A Council of California
- Women's Center - Youth & Family Services → previously_known_as: Prevail (formerly ...)
- Chiefs of Police & Sheriff's Association → NEST-chapter: ...of Alameda County
- Cosmetic, Toiletry and Fragrance Association → NEST-alt: CTFA, ...
- Foothill Transit → NEST-alt: Board of Directors of Foothill Transit
- California Nations Indian Gaming Association → NEST-alt: Cniga - ...
- National Federation of the Blind of California → NEST-chapter: Committee of the...; ...Guide Dog
- American Federation of Musicians → NEST-chapter: ...AFL-CIO (Local 47); NEST-alt: ...of the United States and Canada
- American Federation of Government Employees → NEST-chapter: ...AFL-CIO, District 12; ...Local; NEST-alt: ...AFL-CIO
- Community Residential Care → NEST-alt: Community Residential Care Association of California (Home); ...of
- Jewish Family Service of Los Angeles → NEST-chapter: ...Family Violence Program; ...MSSP Site
- Santa Clara County Office of Education → NEST-chapter: ...Head Start Transition Project; ...Alternative Schools
- California Children's Hospital Association → NEST-chapter: ...San Diego
- California Youth Connection → NEST-chapter: ...Alameda County Chapter
- Foundation for Taxpayer and Consumer Rights → NEST-alt: The Foundation for Taxpayer and Consumer Rights (FTCR); previously_known_as: Consumer Watchdog
- Jewish Federation of Greater Los Angeles → NEST-chapter: ...Jewish Community Relations Committee/JCRC
- Association of Flight Attendants - CWA → NEST-chapter: ...AFL-CIO, Council
- California Federation of Republican Women → NEST-chapter: ...Northern Division
- Civic Center Barrio Housing → NEST-alt: City of San Diego Civic Center Barrio Housing Corporation
- Esperanza Community Housing Corporation → NEST-chapter: ...Fair Housing
- Fullerton Police → NEST-alt: City of Fullerton Police Chief
- Housing Rights Committee → NEST-alt: San Francisco Housing Rights Committee
- Japanese American Citizens League → NEST-chapter: ...San Jose Chapter
- Long Term Care Ombudsman Services of San Luis Obispo County → NEST-alt: California Long Term Care Ombudsman Services of San Luis Obispo County
- Superintendent of Public Instruction → NEST-alt: Office of State Superintendent of Public Instruction [see task 3817]
- U.S. Environmental Protection Agency → NEST-chapter: ...Region IX

---

## CHUNK 8  (task: PHASE2-NEST-8)

### Boy Scouts of America  → NEST-chapter (councils/troops)
Desert Pacific Council; Troop 227; Mt. Diablo Silverado Council; San Francisco Bay Area Council; Monterey Bay Area Council; Pacific Skyline Council; Redwood Empire Council INC.; Boy Scouts of America, San Diego; Old Baldy Council; Boy Scouts of America Forest Home; Boy Scouts of America, San Gabriel Valley; Orange County Council; Golden Gate Area Council; San Gabriel Valley Council; Boy Scouts of America, Ventura County; Sequoia Council; Western Los Angeles County Council; Boy Scouts of America, CA Inland Empire; Troop 807; Greater Los Angeles Area Council; Boy Scouts of America, Desert Pacific; California Inland Empire Council; San Diego - Imperial Council; Boulder Dam Area Council; Southern Sierra; Southern Sierra Council; Los Angeles Area Council; Boy Scouts of America, Troop 730, Diamond Bar; Los Padres Council  (all "..., Boy Scouts of America")

### JUDICIAL COUNCIL  → NEST-alt/chapter
NEST-alt: Judicial Council of California (jury duty provisions); ...(Jury Exemption); CALIFORNIA JUDICIAL COUNCIL; Judicial Council of California two; California Judicial Council, Civil and Small Claims. NEST-chapter: Presiding Judges Advisory Committee of the Judicial Council; Judicial Council Court Administrator's Advisory Committee; Judicial Council of California Administrative Office of the Courts

### California Healthcare Association  → see chunk 7

### JEWISH COMMUNITY RELATIONS COMMITTEE  → see chunk 8 (LA/SF federations) — already listed chunk 7; dedupe on execution

### Smaller anchors (chunk 8)
- Governor's Office → see chunk 7 list
- California Advocates for Nursing Home Reform → NEST-chapter: ...of San Francisco
- Pride at Work → NEST-chapter: S.F. Pride at Work; South Bay Pride at Work; SoCal Pride at Work; NEST-alt: Chapter of Pride at Work
- Battered Women's Alternatives, AIDS Project LA → see chunk 7
- California Chiropractic Association → see chunk 7
- Institute of Scrap Recycling Industries → see chunk 7
- Battered Women's Alternatives → see chunk 7
- Battered Women's / California Building Officials / California Hospital Association / California Union of Safety Employees / Coalition for Adequate School Housing → see chunk 7
- International Brotherhood of Electrical Workers → see chunk 7
- National Action Network → all SEPARATE (conjoined) — skip
- San Diego Fire-Rescue Department → NEST-alt: City of San Diego Fire-Rescue Department; NEST-chapter: ...Lifeguard Services
- Water Replenishment District of Southern California → NEST-chapter: Budget Advisory Committee, ...
- California Association of Persons with Handicaps → see chunk 7
- California Society of Anesthesiologists → see chunk 7
- Little Hoover Commission, National Lawyers Guild, Organization of Chinese Americans → see chunk 2
- Maternal Child and Adolescent Health Action, National Health Law Program, San Bernardino Public Employees, Southwest Healthcare System → see chunk 7
- Battered Women's Alternatives Legal Advocacy Program → see chunk 7

---

## CHUNK 9  (task: PHASE2-NEST-9)

### LEAGUE OF CALIFORNIA CITIES  → NEST-chapter (geographic Divisions + policy depts)
San Diego County Division; Redwood Empire Division; LOS ANGELES COUNTY OF THE LEAGUE OF CALIFORNIA CITIES; Los Angeles Division; San Diego Division; South San Joaquin Valley Division; Riverside County Division; League of California Cities' Housing, Community & Economic  (all "... of the League of California Cities")
(All the "City of X, League of California Cities" conjoined strings = SEPARATE.)

### Million Mom March  → NEST-chapter
California Million Mom March Chapters; Orange County Chapter of the Million Mom March; California Million Mom March, California State Council; Contra Costa Million Mom March; Contra Costa Chapter of the Million Mom March; NEST-alt: Million Mom March United with the Brady Campaign

### National Association of Industrial and Office Properties (NAIOP)  → NEST-chapter
...Orange County; ...CA State Council; ...Inland Empire Chapter; ...SoCal Chapter

### Consumers Union  → NEST-alt/chapter
NEST-alt: Consumers Union-Non-Profit Publishers of Consumer Reports; Consumers Union of U.S., Inc. NEST-chapter: Consumers Union's Safe Patient Project; Sacramento Consumers Union; Consumers Union, Government Relations; ...Government Relations Oversight Committee; Consumers Union, San Francisco

### International Union of Operating Engineers  → NEST-chapter
...Southern California & Southern Nevada; ...(BUs12 and 13: IUOE); Cal-Neva International Union of Operating Engineers

### National Council on Alcoholism and Drug Dependence  → NEST-chapter
...of the San Fernando Valley; ...of the South Bay; ...of East San Gabriel & Pomona Valleys; ...- Orange County

### Smaller anchors (chunk 9)
- Clean Water Action → NEST-alt: Clean Water Action Project; NEST-chapter: Clean Water Action, San Francisco, California; SLO Clean Water Action; Connecticut Clean Water Action/Clean Water Fund; Silicon Valley Clean Water Action (VERIFY)
- Bay Area Air Quality Management District → NEST-chapter: ...Legislative Committee; NEST-alt: Bay Area Air Quality Management Districts
- California Peace Officers Association → NEST-alt: California Peace Officers' Association (in principle / approve in principle / disapprove in principle); NEST-chapter: ...Government Relations; ...Governmental; Northern California Peace Officers Association (VERIFY)
- California Public Utilities Commission → NEST-chapter: Advocates-CPUC (Office of Ratepayer Advocates); California Public Utilities Commission Office of Ratepayer
- San Diego Association of Governments → NEST-chapter: ...Public Safety Working Group; ...Advisory Committee; Advisory Committee to the Multiple Habitat Conservation Plan, ...; ...Executive Committee
- American Council of Engineering Companies → NEST-chapter: Los Angeles County Chapter of ...; ...of Northern California [see task 3820]
- Alliance of Catholic Health Care → NEST-alt: Alliance of Catholic Health Care Systems
- Children's Hospital Oakland → NEST-alt: USCF Benioff Children's Hospital Oakland [previously_known_as]; NEST-chapter: Children's Hospital, Oakland, Department of Psychiatry
- San Francisco Board of Supervisors → NEST-chapter: ...Public Safety Committee; ...Food Security Task Force [SF gov also in task 3816]
- American College of Obstetricians and Gynecologists - District IX → NEST-alt: The American College...District IX (ACOG); ...District IX (ACOG-IX)
- Bend the Arc → NEST-chapter: Bend the Arc Bay Area
- California Cattlemen's Association → NEST-chapter: ...Humboldt County
- Council for Environmental and Economic Balance → NEST-alt: Calif. Council for Environmental and Economic Balance
- First Amendment Coalition → NEST-alt: CA First Amendment Coalition
- Irvine Ranch Water District → NEST-alt: The Irvine Ranch Water District (IRWD); City of Irvine Ranch Water District (VERIFY)
- National Federation of Independent Business → NEST-chapter: NFIB California
- Santa Monica Malibu Unified School District → NEST-chapter: ...Adult Education; ...Board of Education
- Women's Foundation of California - Women's Policy Institute → NEST-chapter: ...Trauma Justice Team; ...Criminal Justice Team [see chunk 6]
- Association of Environmental Health Administrators → NEST-chapter: Western Association of Environmental Health Administrators (VERIFY)
- Children's Law Center of California → NEST-alt: Children's Law Center of California (Los Angeles, Placer, Sacramento)
- East Bay Asian Local Development Corporation → NEST-alt: City of East Bay Asian Local Development Corporation
- International Union of Painters and Allied Trades, Painters & Tapers → NEST-alt: ...Painters, Tapers, Floorcoverers, & Glaziers [see task 3819]
- Merck and Company → NEST-chapter: Kelco Division of Merck and Company, Inc
- Social Compassion in Legislation → NEST-alt: Social Compassion in Legislation (SCIL) (SRAP)
- Speech-Language Pathology and Audiology and Hearing Aid Dispensers Board → NEST-alt: Department of Consumer Affairs, ...
- Twin Cities Police Authority → NEST-alt: City of Twin Cities (Police Authority)
- Yuba Community College District → NEST-chapter: ...Public Safety Center

---

## CHUNK 10  (task: PHASE2-NEST-10)

### LEAGUE OF UNITED LATIN AMERICAN CITIZENS (LULAC)  → NEST-chapter (councils/districts)
...Pajaro Valley Council; ...District XIII; - Council 3272; ...Council 328; ...Richmond; ...Mayfair Council; - District 7; Santa Maria ...Council 3252; ...Council 3288; - Far West Region; Santa Ana ...; Antelope Valley ...; Santa Maria ...Council 3187; ...Santa Clara Council; San Benito County ...Council 2890; ...the Lorenzo; ...Santa Ana LULAC Council 147; ...Youth Organization; Sacramento ...Lorenzo; California ...(LULAC) Housing; California ...Public Interest Advocacy
→ NEST-chapter (state-level bodies): CALIFORNIA LEAGUE OF UNITED LATIN AMERICAN CITIZENS; CA League of United Latin American Citizens (LULAC); ...- California State; ...- California Chapter; California Lulac State Organization - ...; The League of United Latin American Citizens California Chapters
→ NEST-alt: LULAC, League of United Latin American Citizens

### AMALGAMATED TRANSIT UNION  → NEST-chapter/alt  [ATU also in task 3818]
NEST-chapter: ...Local Union 1574; ...Local No. 1575; CA Conference Board of the ATU; Board of the ATU; California Board of the ATU; California State Association of ATU; CA Conf of ATU; California Conference Board of the ATU, AFL-CIO; Conference Board of the ATU; ATU Locals 1555 and 1574. NEST-alt: ATU Members; ATU, AFL-CIO

### California Highway Patrol  → NEST-chapter/alt (area offices only)
NEST-alt: Department of the California Highway Patrol; State Department of the California Highway Patrol. NEST-chapter: California Highway Patrol Monterey Area; Santa Maria California Highway Patrol; Department of California Highway Patrol, Barstow Area

### San Francisco Public Defender  → NEST-alt/chapter
NEST-alt: San Francisco Public Defender Jeff Adachi; Manohar Raju - San Francisco Public Defender; San Francisco Public Defender Office; San Francisco Public Defender's Officer. NEST-chapter: Racial Justice Committee, San Francisco Public Defender Office

### Smaller anchors (chunk 10)
- Health Access → NEST-alt: Health Access Foundation (all other "Health Access" strays = distinct orgs, SEPARATE)
- Children's Advocacy Institute → NEST-alt: Conference: Children's Advocacy Institute; ...- San Diego School of Law; ...(CAI) at the University of San Diego School; ...of the University of San Diego
- Coalition for Clean Air → NEST-alt: CA Coalition for Clean Air
- AIDS Healthcare Foundation → NEST-alt: The AIDS Healthcare Foundation (AHF); NEST-chapter: ...Housing is a Human Right Division
- California Senior Legislature → NEST-alt: ...- State of California; NEST-chapter: Joint Rules Committee of the...; San Diego Caucus, ...
- SEMPRA Energy → NEST-chapter: SEMPRA ENERGY SOLUTIONS [Sempra also in task 3818]
- California State Council of Laborers → NEST-alt: ...Legislative; ...Legislative Department; ...- Laborers International Union of North America
- American Federation of Government Employees → NEST-chapter: ...AFL-CIO, District 12; ...Local; NEST-alt: ...AFL-CIO
- Community Residential Care → NEST-alt: ...Association of California (Home); ...of
- Jewish Family Service of Los Angeles → NEST-chapter: ...Family Violence Program; ...MSSP Site
- Santa Clara County Office of Education → NEST-chapter: ...Head Start Transition Project; ...Alternative Schools
- American Federation of Musicians → NEST-chapter: ...AFL-CIO; NEST-alt: ...of the United States and Canada
- California Children's Hospital Association → NEST-chapter: ...San Diego
- California Youth Connection → NEST-chapter: ...Alameda County Chapter
- Foundation for Taxpayer and Consumer Rights → previously_known_as: Consumer Watchdog
- Jewish Federation of Greater Los Angeles → NEST-chapter: ...JCRC
- National Federation of the Blind of California → NEST-chapter: Committee of the...; ...Guide Dog
- Civic Center Barrio Housing → NEST-alt: City of San Diego Civic Center Barrio Housing Corporation
- Esperanza Community Housing Corporation → NEST-chapter: ...Fair Housing
- Fullerton Police → NEST-alt: City of Fullerton Police Chief
- Housing Rights Committee → NEST-alt: San Francisco Housing Rights Committee
- Japanese American Citizens League → NEST-chapter: ...San Jose Chapter
- U.S. Environmental Protection Agency → NEST-chapter: ...Region IX
