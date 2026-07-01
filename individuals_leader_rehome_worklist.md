# Individuals-CSV → Leader Re-home Worklist

Generated 2026-07-01 by the management assistant via a 20-agent parallel audit of
`org_name_subsets_for_cleaning/org_names_that_are_actually_individuals.csv` (9,653 entries).

Each line below is an entry currently in the individuals CSV that is actually a PERSON IN A TOP
LEADERSHIP ROLE representing an identifiable organization. Per project rules (see CLAUDE.md +
memory `named_officials_per_case`), a leadership title (Mayor / Mayor Pro Tem / President / CEO /
Chief / Sheriff / Superintendent / Board Chair / Founder / Owner / Pastor-Rabbi / etc.) makes the
entry an ALTERNATE SPELLING of the org it represents — it belongs in the crosswalk, not in individuals.

Line format: `- <exact CSV org_name>,<count> | <role> | <organization to consolidate under>`

## RA WORKFLOW for each assigned line:
1. Take the org named in the third field (`| <organization>`). SEARCH the crosswalk for it first
   (e.g. "City of X", the school district, the police/sheriff dept, the company, the county Board
   of Supervisors, the CA State Board of Equalization, etc.). It almost always already exists.
2. Add the EXACT CSV org_name string as an `alternate_spelling` child under that canonical
   (create the canonical only if it genuinely isn't anywhere in the crosswalk).
3. MOVE the CSV row (org_name + count) from
   `org_name_subsets_for_cleaning/org_names_that_are_actually_individuals.csv`
   to `org_name_subsets_for_cleaning/org_names_in_crosswalk.csv`.
4. Entries tagged `MISFILED-ORG` are not people at all — add the org to the crosswalk as a canonical
   (or alt of an existing one) and move the row to `org_names_in_crosswalk.csv`.
5. VERIFY suspicious lines (marked `[verify]` / `[NOTE ...]`): if the string looks internally
   corrupt or name/city-misaligned (e.g. a mayor name paired with the wrong city), map to the city
   NAMED IN THE STRING; if genuinely conjoined/garbled, route to conjoined/narrative CSV instead and note it.
6. Run the clean/dedup/stats pipeline before committing. Follow the Data Write Queue.

---


## chunk_01
- Johan Klehs, Chair State Board of Equalization,13 | Chair | California State Board of Equalization
- Chief of Police Phillip Green, Cities of Corte Madera/Larkspur,7 | Police Chief | Central Marin Police Authority
- Dennis Rodoni, Board of Supervisors President, Marin County,6 | Board President | Marin County Board of Supervisors
- James Breitling, Mayor Pro Tem, Upland,6 | Mayor Pro Tem | City of Upland
- Mark Enmeier, Mayor Pro-tem, San Clemente,6 | Mayor Pro Tem | City of San Clemente

## chunk_02
- Santa Ana Police Chief Paul Walters,6 | Police Chief | Santa Ana Police Department
- David Armenta Mayor of Redwood City,5 | Mayor | City of Redwood City
- Rev. John Anderson, St. John's Presbyterian Church-San,5 | Pastor | St. John's Presbyterian Church
- Cathedral City Mayor Pro Tem Greg Pettis,5 | Mayor Pro Tem | Cathedral City
- Los Angeles Unified School District, Board President Jose,4 | Board President | LAUSD

## chunk_03
- Mayor Alma Beltran, Parlier,4 | Mayor | City of Parlier
- City of Carlsbad Mayor Art Madrid,3 | Mayor | City of Carlsbad
- City of Carlsbad Mayor Jerry Sanders,3 | Mayor | City of Carlsbad
- City of Del Mar Mayor Claude A. "Bud" Lewis,3 | Mayor | City of Del Mar
- City of El Cajon Mayor Mary Teresa Sessom,3 | Mayor | City of El Cajon
- City of Escondido Mayor Mark Lewis,3 | Mayor | City of Escondido
- City of La Mesa Mayor Carl Hilliard,3 | Mayor | City of La Mesa
- City of Lemon Grove Mayor Mickey Cafagna,3 | Mayor | City of Lemon Grove
- City of Poway Mayor Randy Voepel,3 | Mayor | City of Poway
- City of San Diego Mayor Lesa Heebner,3 | Mayor | City of San Diego [NOTE: likely Solana Beach, verify]
- City of Santee Mayor Tom Smisek,3 | Mayor | City of Santee
- City of Solana Beach Mayor Lori Holt Pfeiler,3 | Mayor | City of Solana Beach
- Carmella S. Franco, Superintendent, Whittier City School,3 | Superintendent | Whittier City School District
- Acting Sheriff of Stanislaus,3 | Sheriff | Stanislaus County Sheriff's Office

## chunk_04
- San Anselmo Mayor Steve Burdo,3 | Mayor | City of San Anselmo
- David M. Solomon, Owner, Cash & Advance of California,3 | Owner | Cash & Advance of California
- Johan Klehs, Chair of the State Board of Equalization,3 | Chair | California State Board of Equalization
- Johan Klehs, Chair, Board of Equalization,3 | Chair | California State Board of Equalization
- Phil Serna, Sacramento County Board of Supervisors, Chair,3 | Board Chair | Sacramento County Board of Supervisors

## chunk_05
- Capistrano Mayor, David Swerdlin,2 | Mayor | City of San Juan Capistrano
- Caplan, Founder and Chairman of Frieda's,2 | Founder/Chairman | Frieda's Specialty Produce
- Charles H. Boxenbaum, Chairman of the Board, National Partnership,2 | Chairman | National Partnership [org truncated]
- City of Fontana, Mayor Pro Tem, John Roberts,2 | Mayor Pro Tem | City of Fontana
- Dave Malcolm, President of Southern California Restaurant,2 | President | Southern California Restaurant Association [truncated]
- Deanne Tilton Durfee, Chair, U.S. Advisory Board on Child Abuse and Neglect,2 | Chair | U.S. Advisory Board on Child Abuse and Neglect
- Donald Beall, Chief Executive Officer, Rockwell International,2 | CEO | Rockwell International
- Dr. Kevin Starr, Chair of the Sesquicentennial Commission,2 | Chair | Sesquicentennial Commission
- El Monte City Mayor Andre,2 | Mayor | City of El Monte
- Former Mayor of the City of Huntington Park,2 | Mayor | City of Huntington Park

## chunk_06
- Garrett Huff ... President of Santa Barbara County Fire Fighter's Benevolent Foundation,2 | President | SB County Fire Fighter's Benevolent Foundation
- John Chiang ... Chair of California Pollution Control Financing Authority,2 | Chair | CA Pollution Control Financing Authority
- Joseph McNamara ... Police Chief, San Jose (Ret.),2 | Police Chief | San Jose Police Department
- Kathleen Connell, Controller and Chair of California State Lands Commission,2 | Chair | CA State Lands Commission
- Mayor Bill Bogaard City of Walnut Creek,2 | Mayor | City of Walnut Creek [NOTE: Bogaard=Pasadena, verify]
- Mayor Cindy Silva City of West Hollywood,2 | Mayor | City of West Hollywood [NOTE: Silva=Walnut Creek, verify]
- Mayor Rob Schroder City of Pasadena,2 | Mayor | City of Pasadena [NOTE: Schroder=Martinez, verify]
- Reverend Neil Thomas, Senior Pastor, Metropolitan Community,2 | Senior Pastor | Metropolitan Community Church
- Sheriff Gary Penrod of San Bernardino,2 | Sheriff | San Bernardino County Sheriff's Department

## chunk_07
- The Office of Governor Gray Davis,2 | Governor's Office | Office of the Governor of California [MISFILED-ORG / governmental office]

## chunk_09
- Alan Carlson, Chief Executive Officer, San Francisco Superior,1 | CEO | San Francisco Superior Court
- Alan Ishida, Chairman, Tulare County Board,1 | Chairman | Tulare County Board of Supervisors
- Ali Hasan, President, Parchester Village Neighborhood Council,1 | President | Parchester Village Neighborhood Council
- Andaz West Hollywood General Manager Lin,1 | General Manager | Andaz West Hollywood
- Angeles Mayor Richard,1 | Mayor | City of Los Angeles (Riordan)
- Angeles, Mayor Antonio Villaraigosa,1 | Mayor | City of Los Angeles
- Audrey Clarke, Superintendent, Lynwood School District,1 | Superintendent | Lynwood School District
- Barbara Mayor Helene Schneider,1 | Mayor | City of Santa Barbara
- Barry Newman, Chairman, San Diego County Treasurer Oversight,1 | Chairman | SD County Treasurer Oversight Committee
- Beverly Roher, Superintendent, Redondo Beach School District,1 | Superintendent | Redondo Beach School District
- Bill Uphoff - Mayor Pro Tem of Lomita,1 | Mayor Pro Tem | City of Lomita
- Captain Monty Ashliman Mayor of Ridgecrest,1 | Mayor | City of Ridgecrest
- Carlos Manuel Martins Do Vale Cesar, President of the Regional Government of the Azores,1 | President | Regional Government of the Azores
- Casey Gwinn, Chair, Attorney General's Task Force,1 | Chair | Attorney General's Task Force

## chunk_12
- El Cajon Mayor Pro Tem Bill Wells,1 | Mayor Pro Tem | City of El Cajon
- Emeryville Mayor Dianne Martinez,1 | Mayor | City of Emeryville
- Eric Garcetti, Mayor of Los,1 | Mayor | City of Los Angeles
- Eric Oster, Founder of the Road Angels,1 | Founder | The Road Angels
- Fiona Ma, Board of Equalization Chair, and Member Second District,1 | Chair | CA State Board of Equalization
- Gabriel Quinto- Mayor Pro Tem City of El Cerrito,1 | Mayor Pro Tem | City of El Cerrito
- Garcetti, Mayor of the City of Los Angeles,1 | Mayor | City of Los Angeles
- Gregory S. Pettis, Mayor Pro Tem, Cathedral City Council,1 | Mayor Pro Tem | City of Cathedral City

## chunk_13
- John M Erickson, Phd - West Hollywood Mayor Pro Tempore,1 | Mayor Pro Tem | City of West Hollywood
- Justin Massey - Mayor Pro Tempore of Hermosa Beach,1 | Mayor Pro Tem | City of Hermosa Beach
- Jacinto Mayor Alonzo Ledezma,1 | Mayor | City of San Jacinto
- Ken Nelson Aquatech Pools,1 | Owner | Aquatech Pools
- Lancaster Burns Construction Inc. Lawrence W,1 | MISFILED-ORG | Burns Construction Inc.

## PENDING: chunks 08, 10, 11, 14, 15, 16, 17, 18, 19, 20

## chunk_08
- County of Fresno Board of Supervisors Chair Sal Quintero,2 | Board Chair | County of Fresno Board of Supervisors
- Al Rich, CEO, Solarroofs,2 | CEO | Solarroofs
- Alvin Duskin, CEO, Clean Coal Energy,2 | CEO | Clean Coal Energy
- Andrew Beebe, President, Energy Innovations,2 | President | Energy Innovations
- Andrew Galef, CEO, MagneTek, Inc,2 | CEO | MagneTek, Inc
- Andy Funk, CEO ... Funk Ventures,2 | CEO | Funk Ventures
- Brian Dougherty, CEO, Airena, Inc,2 | CEO | Airena, Inc
- David l. Ortiz, Principal, La Colina Junior High School,2 | Principal | La Colina Junior High School
- Dr. Eric Schmidt ... CEO, Novell, Inc,2 | CEO | Novell, Inc
- Dr. Frank Ellsworth, President, Independent Colleges of So,2 | President | Independent Colleges of Southern California
- Vanessa Quiroz-carter - Mayor Pro Tempore of Watsonville,2 | Mayor Pro Tem | City of Watsonville
- Johan Klehs BOE Member and Chairman,2 | Chairman | CA State Board of Equalization
- Dean Andal, Chair, State Board of Equalization,2 | Chair | CA State Board of Equalization
- L. Blitch, Chair, San Francisco Chamber of Commerce,2 | Chair | San Francisco Chamber of Commerce
- Marty Kudlak, Former Mayor of Atascadero,2 | Mayor | City of Atascadero
- Mike Arrambide, Former Mayor of Atascadero,2 | Mayor | City of Atascadero
- Office of the Mayor of the City of Bakersfield/ The Bakersfield City Council,2 | MISFILED-ORG | govt bodies
- Prospect Medical Group, Culver City (Jacob Y. Terner, MD),2 | MISFILED-ORG | Prospect Medical Group

## chunk_10
- City of Bakersfield (prior version) Mayor Emily Gabel-Luddy,1 | Mayor | City of Bakersfield
- City of Bakersfield Mayor Kevin Johnson,1 | Mayor | City of Bakersfield [verify name]
- City of Burbank (prior version) Mayor Kevin Johnson,1 | Mayor | City of Burbank [verify]
- City of Sacramento (prior version) Mayor Chuck Reed,1 | Mayor | City of Sacramento [verify]
- City of Sacramento Mayor Chuck Reed,1 | Mayor | City of Sacramento [verify]
- City of San Jose (prior version) Mayor Miguel Pulido,1 | Mayor | City of San Jose [verify]
- City of San Jose Mayor Miguel Pulido,1 | Mayor | City of San Jose [verify]
- City of Santa Ana (prior version) Mayor Edwin M. Lee,1 | Mayor | City of Santa Ana [verify]
- City of Santa Ana Mayor Edwin M. Lee,1 | Mayor | City of Santa Ana [verify]
- Christian Dinco - Mayor Pro Tempore of Eastvale,1 | Mayor Pro Tem | City of Eastvale
- City of Downey Mayor Pro Tem Mario Trujillo,1 | Mayor Pro Tem | City of Downey
- City of Fairfield Mayor pro Tem Rosemary Ramirez,1 | Mayor Pro Tem | City of Fairfield
- City of Hayward Mayor Pro Tem Sara Lamin,1 | Mayor Pro Tem | City of Hayward
- City of Santa Ana Mayor Pro Tem Jessie Lopez,1 | Mayor Pro Tem | City of Santa Ana
- Connie Rogers, Gilroy (Mayor Pro Tem),1 | Mayor Pro Tem | City of Gilroy
- Chief of Police Byron Nelson, Azusa,1 | Police Chief | Azusa Police Department
- Chief of Police, Randy Narramore of the City of Ridgecrest,1 | Police Chief | Ridgecrest Police Department
- Contra Costa County Sheriff David,1 | Sheriff | Contra Costa County Sheriff's Office
- Contra Costa Sheriff Warren Rupf,1 | Sheriff | Contra Costa County Sheriff's Office
- City Manager Janet Dolan, Menlo Park,1 | City Manager | City of Menlo Park

## chunk_11
- David Armenta, Mayor of Pico Rivera,1 | Mayor | City of Pico Rivera
- David John Shawver, Mayor of Stanton,1 | Mayor | City of Stanton
- David Rabbitt, Sonoma County Board of Supervisors, Chair,1 | Board Chair | Sonoma County Board of Supervisors
- Deasy, Superintendent of Los Angeles School District,1 | Superintendent | LAUSD
- Douglas Qualls, Fire Chief, Apple Valley,1 | Fire Chief | Apple Valley Fire Protection District
- Dr. Gwen Gross, Superintendent, Hermosa Beach School District,1 | Superintendent | Hermosa Beach City SD

## chunk_14
- Mayor Andrew Weissman City of Martinez,1 | Mayor | City of Martinez [verify:Weissman=Glendale]
- Mayor David E. Durant of Pleasant Hill,1 | Mayor | City of Pleasant Hill
- Mayor Gary Miller, City of Diamond Bar,1 | Mayor | City of Diamond Bar
- Mayor Jeffrey Cooper City of Martinez,1 | Mayor | City of Martinez [verify:Cooper=Culver City]
- Mayor Pro Tem Charles England (Cathedral City),1 | Mayor Pro Tem | City of Cathedral City
- Mayor Pro Tem Gabriel Quinto, El Cerrito,1 | Mayor Pro Tem | City of El Cerrito
- Mayor Pro Tempore Richard Constantine, City or Morgan Hill,1 | Mayor Pro Tem | City of Morgan Hill
- Mayor Pro Tempore of the City of Morgan Hill,1 | Mayor Pro Tem | City of Morgan Hill
- Mayor Willie Brown, City and County of San,1 | Mayor | City and County of San Francisco
- Mayor of Atascadero,1 | Mayor | City of Atascadero
- Mayor of San Jose Colleen B. Wilcox,1 | Mayor | City of San Jose [verify]
- Mayor of Waterford Honorable Acquanetta Warren,1 | Mayor | City of Waterford [verify:Warren=Fontana]
- Mayor of the City of Oakland Henry Chang,1 | Mayor | City of Oakland
- Misty Perez - Mayor Pro Tempore of Port Hueneme,1 | Mayor Pro Tem | City of Port Hueneme
- Norby, Mayor Pro Tem of the City of Fullerton,1 | Mayor Pro Tem | City of Fullerton
- Oakland, Mayor Ronald Dellums,1 | Mayor | City of Oakland
- Palma Mayor Steve Hwangbo,1 | Mayor | City of La Palma
- Mr. Michael Marmot, CEO, OPTIONS,1 | CEO | OPTIONS

## chunk_16
- The Honorable Jerry Brown, Mayor of Oakland,1 | Mayor | City of Oakland
- William Graber (Georgiana Ranch Nursery),1 | Owner | Georgiana Ranch Nursery

## chunk_17
- Acting Sheriff of Stanislaus County,1 | Sheriff | Stanislaus County Sheriff's Office
- Alan Ishida, Chairman, Tulare County Board of Supervisors,1 | Chairman | Tulare County Board of Supervisors
- Chris Norby, Mayor Pro Tem of the City of Fullerton,1 | Mayor Pro Tem | City of Fullerton
- Chair of the Sesquicentennial Commission Mr. Don Geiger,1 | Chair | Sesquicentennial Commission
- County of Stanislaus, District 4 Supervisor (Chairman), Ray Simon,1 | Board Chair | Stanislaus County Board of Supervisors
- David Rabbitt, Sonoma County Board of Supervisors, Chair and Second District Supervisor,1 | Board Chair | Sonoma County Board of Supervisors
- Dennis Osmer, former Mayor of Watsonville,1 | Mayor | City of Watsonville

## chunk_18
- Joel Makower, Founder, Greenbiz.com,1 | Founder | GreenBiz
- Michael J. Beck, City Manager, Pasadena,1 | City Manager | City of Pasadena

## PENDING: chunks 15, 19, 20

## chunk_15
- Redlands Mayor Pro Tempore Jon Harrison,1 | Mayor Pro Tem | City of Redlands
- Richard Riordan, Mayor, City of,1 | Mayor | City of Los Angeles
- Sacramento Mayor Joe,1 | Mayor | City of Sacramento
- Sara Lamin, Mayor Pro Tem City of Hayward,1 | Mayor Pro Tem | City of Hayward
- Shirley Horton, Mayor of Chula,1 | Mayor | City of Chula Vista
- Shirley Horton, Mayor of the City,1 | Mayor | City of Chula Vista
- Sheriff Lee Baca, Los,1 | Sheriff | Los Angeles County Sheriff's Department
- Supervisor Greg Cox, Chairman, San Diego County Board of Supervisors,1 | Board Chair | San Diego County Board of Supervisors
- Supervisor Terrance P. Withrow, Chairman, Stanislaus County Board of Supervisors,1 | Board Chair | Stanislaus County Board of Supervisors
- Susan Sonne - Mayor Pro Tempore of Buena Park,1 | Mayor Pro Tem | City of Buena Park
- The Honorable Antonio R. Villaraigosa, Mayor City,1 | Mayor | City of Los Angeles
- The Honorable Bill Bogaard, Mayor City of Pasadena,1 | Mayor | City of Pasadena
- San Francisco District Attorney's,1 | MISFILED-ORG | SF District Attorney's Office
- San Francisco District Attorney, Victim Witness Assistance,1 | MISFILED-ORG | SF DA Victim Witness Assistance
- San Francisco Mayor's Criminal Justice,1 | MISFILED-ORG | SF Mayor's Office of Criminal Justice

## PENDING: chunks 19, 20

## chunk_19
- Tim Ryan, Chief of Correction, Santa Clara County Department of Correction,1 | Dept head | Santa Clara County Dept of Correction
- Newport Beach Police Chief Bob McDonnell,1 | Police Chief | Newport Beach Police Department
- Stanislaus County, Library Director, Vanessa Czopek,1 | Library Director | Stanislaus County Library
- Fiona Ma, Chairwoman, California State Board of Equalization,1 | Chair | CA State Board of Equalization
- Judy Chu, Chair, Board of Equalization,1 | Chair | CA State Board of Equalization
- Judy Chu, Chair, State Board of Equalization,1 | Chair | CA State Board of Equalization
- Hon. Johan Klehs, Chairman, Board of Equalization,1 | Chair | CA State Board of Equalization
- Honorable Johan Klehs, Chairman, Board of Equalization,1 | Chair | CA State Board of Equalization
- The Honorable Johan Klehs, Chair, Board of Equalization,1 | Chair | CA State Board of Equalization
- Mr. William F. Ardizzone, President, Long Beach Firefighters,1 | President | Long Beach Firefighters (IAFF local)
- Cottonwood Chamber of Commerce President Tim Bork,1 | President | Cottonwood Chamber of Commerce
- Valley Ball Cabaret,1 | MISFILED-ORG | business/venue

## chunk_20
- Mike Lane, Police Chief, UC Riverside,1 | Police Chief | UC Riverside Police Department
- Patty Borelli, Chair - El Dorado County Transportation Commission,1 | Chair | El Dorado County Transportation Commission
- Phil Larson, Chairman Fresno County Board of Supervisors,1 | Chairman | Fresno County Board of Supervisors
- Phill Carter, Founder, Environment.wiki,1 | Founder | Environment.wiki
- Russell Lucio, Owner, Auto Registration,1 | Owner | Auto Registration
- Rabbi Abner Weiss, Beth Jacob Congregation,1 | Rabbi | Beth Jacob Congregation
- Rabbi Abner Weiss, Beth Jacob Congregation Beverly Hills,1 | Rabbi | Beth Jacob Congregation (Beverly Hills)
- Rabbi Mendel Weinfeld, The Chabad House (San Jose),1 | Rabbi | The Chabad House (San Jose)
- Rabbi at Beth Jacob Congregation,1 | Rabbi | Beth Jacob Congregation
- Robert Garcia, Police Chief, Canyon City,1 | Police Chief | Canyon City Police Department
- ERIC GARCETTI, MAYOR, CITY OF LOS ANGELES,0 | Mayor | City of Los Angeles

## END
