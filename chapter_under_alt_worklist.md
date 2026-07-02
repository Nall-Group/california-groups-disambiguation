# Chapter-nested-under-alternate_spelling cases

Generated 2026-07-01. Each case: a node marked `alternate_spelling` that wrongly OWNS `chapter` children.

## RA per-case decision
An `alternate_spelling` is the same entity as its parent canonical, so it must not own chapters. For EACH case decide:
- (A) If the ALT is TRULY just a spelling variant of the canonical -> RE-PARENT its chapter child(ren) up to be `chapter` children of the CANONICAL (siblings of the alt).
- (B) If the ALT is actually a DISTINCT sub-entity mislabeled as an alt (a state/regional federation, a local, a division -- e.g. `California Federation of Teachers` under AFT, `Operating Engineers Local Union No. 3` under IUOE) -> change ITS relationship from `alternate_spelling` to `chapter`, and KEEP its chapter child(ren) nested under it.
Dedup after moving. Within-crosswalk reorg (no CSV moves). Run the clean/dedup/stats pipeline before committing. Follow the Data Write Queue.

---

1. CANONICAL `Academic Senate for the California Community Colleges`
     alt-with-chapters: `Laney College`
     chapter child(ren): ['Academic Senate, Laney College']

2. CANONICAL `American Federation of Teachers`
     alt-with-chapters: `California Federation of Teachers`
     chapter child(ren): ['Lawndale Federation of Classified Employees - Local 4529']

3. CANONICAL `Anthem Blue Cross (Blue Cross)`
     alt-with-chapters: `Blue Cross`
     chapter child(ren): ['Blue Cross of CA']

4. CANONICAL `Bar Association`
     alt-with-chapters: `State Bar Association`
     chapter child(ren): ['State Bar Association of California']

5. CANONICAL `BreastfeedLA: The Breastfeeding Task Force of Greater Los Angeles`
     alt-with-chapters: `Breastfeeding Task Force of Greater Los Angeles`
     chapter child(ren): ['Breastfeeding Task Force of Greater Los']

6. CANONICAL `CALIFORNIA ASSOCIATION OF PROFESSIONAL SCIENTISTS`
     alt-with-chapters: `Association of Professional Scientists`
     chapter child(ren): ['California Association of Professional Scientists, UAW Local 1115']

7. CANONICAL `City of Los Angeles`
     alt-with-chapters: `Angeles City`
     chapter child(ren): ['Angeles City, Local 112']

8. CANONICAL `Doris Tate Crime Victims Bureau`
     alt-with-chapters: `Doris Tate Crime Victimos Bureau`
     chapter child(ren): ['Doris Tate Crime Victimos Bureau, California']

9. CANONICAL `Douglas Hardwood Company, Rancho Santa Fe`
     alt-with-chapters: `Douglas Harwood Company, Rancho Santa Fe`
     chapter child(ren): ['Douglas Harwood Company']

10. CANONICAL `ENGINEERS AND SCIENTISTS OF CALIFORNIA`
     alt-with-chapters: `Scientists of California`
     chapter child(ren): ['Scientists of California, Local 20']

11. CANONICAL `First African Methodist Episcopal Church`
     alt-with-chapters: `First AME Church`
     chapter child(ren): ['First AME Church of Los Angeles']

12. CANONICAL `Friends Committee on Legislation of California`
     alt-with-chapters: `Friends Committee on Legislation`
     chapter child(ren): ['Friends Committee on Legislation California', 'Local 1000 Friends Committee on Legislation']

13. CANONICAL `Global Green`
     alt-with-chapters: `Global Green USA`
     chapter child(ren): ['GLOBAL GREEN USA (SANTA MONICA)']

14. CANONICAL `International Association of Fire Fighters`
     alt-with-chapters: `International Assn. of Fire Fighters`
     chapter child(ren): ['International Assn. of Firefighters - Local 55']

15. CANONICAL `INTERNATIONAL UNION OF OPERATING ENGINEERS`
     alt-with-chapters: `Operating Engineers Local Union No. 3`
     chapter child(ren): ['Operating Engineers Local Union No. 3 (Alameda)']

16. CANONICAL `Mono County`
     alt-with-chapters: `COUNTY OF MONO`
     chapter child(ren): ['County of Mono, California']

17. CANONICAL `NAIOP of California`
     alt-with-chapters: `NAIOP - Commercial Real Estate Development Association`
     chapter child(ren): ['NAIOP - Commercial Real Estate Development Association of California', 'Commercial Real Estate Development Association, Naiop Of California', 'NAIOP, Silicon Valley Chapter']

18. CANONICAL `NAIOP of California`
     alt-with-chapters: `National Association for Industrial and Office Parks`
     chapter child(ren): ['National Association for Industrial and Office Parks California']

19. CANONICAL `Napa Valley Vintners`
     alt-with-chapters: `NAPA VALLEY VINTNERS ASSOCIATION`
     chapter child(ren): ['Napa Valley Vintners Association, California']

20. CANONICAL `National Abortion Rights Action League`
     alt-with-chapters: `California Abortion and Reproductive Rights Action League, North & South`
     chapter child(ren): ['California Abortion and Reproductive Rights Action League-North']

21. CANONICAL `NATIONAL ALLIANCE ON MENTAL ILLNESS`
     alt-with-chapters: `NAMI - California`
     chapter child(ren): ['NAMI California (National Association of)', 'NAMI - Nevada County', 'NAMI California Consumer Council', 'NAMI Alameda County South', 'NAMI Amador', 'NAMI Butte County', 'Nami Coachella Valley', 'NAMI Contra Costa County', 'NAMI East San Gabriel Valley', 'NAMI Fresno', 'Nami Greater Los Angeles County', 'NAMI Humboldt', 'NAMI Inland Valley', 'NAMI Kern County', 'NAMI Lassen', 'NAMI Los Angeles', 'NAMI Los Angeles County Council', 'NAMI Mendocino County', 'NAMI Mt. San Jacinto', 'NAMI Northern Santa Barbara County', 'NAMI Orange County', 'NAMI Pomona Valley', 'NAMI Sacramento', 'NAMI San Bernardino Area', 'NAMI San Diego County', 'NAMI-North Coastal San Diego County', 'NAMI San Francisco', 'NAMI San Gabriel Valley', 'NAMI San Joaquin County', 'NAMI San Luis Obispo County', 'NAMI San Mateo County', 'NAMI Santa Barbara', 'Nami Santa Clara County', 'NAMI Santa Cruz', 'NAMI Solano County', 'NAMI Sonoma County', 'NAMI South Bay', 'NAMI Southern Santa Barbara County', 'NAMI Tuolomne County', 'NAMI Urban Los Angeles', 'NAMI Ventura County', 'NAMI Whittier', 'NAMI Yolo County', 'NAMI Yuba-Sutter']

22. CANONICAL `NATIONAL COUNCIL OF JEWISH WOMEN`
     alt-with-chapters: `National Council of Jewish Women-San Francisco Bay Area Section`
     chapter child(ren): ['National Council of Jewish Women/LA']

23. CANONICAL `NATIONAL ORGANIZATION FOR WOMEN`
     alt-with-chapters: `California National Organization for Women`
     chapter child(ren): ['California National Organization for Women, San Gabriel Valley']

24. CANONICAL `North America's Building Trades Unions`
     alt-with-chapters: `STATE BUILDING AND CONSTRUCTION TRADES COUNCIL`
     chapter child(ren): ['STATE BUILDING AND CONSTRUCTION TRADES COUNCIL OF CALIFORNIA', 'Local 105 State Building and Construction Trades Council', 'State Building Trades Council of California', 'California State Building Trades Council', 'State Building and Trades Council of California', 'State Buildings and Construction Trades Council of California']

25. CANONICAL `North America's Building Trades Unions`
     alt-with-chapters: `Building and Construction Trades Council`
     chapter child(ren): ['Building and Construction Trades Council of California']

26. CANONICAL `North America's Building Trades Unions`
     alt-with-chapters: `State Building & Construction Trade Council`
     chapter child(ren): ['State Building & Construction Trade Council of California']

27. CANONICAL `Office of State Controller`
     alt-with-chapters: `State Controller`
     chapter child(ren): ['State Controller of California']

28. CANONICAL `Orange County Employees Association (OCEA)`
     alt-with-chapters: `Orange County Employees Association`
     chapter child(ren): ['Local 685 Orange County Employees Association']

29. CANONICAL `PENINSULA CORRIDOR JOINT POWERS BOARD`
     alt-with-chapters: `Peninsula Corridor Joint Powers Authority`
     chapter child(ren): ['Peninsula Corridor Joint Powers Authority (Caltrain)']

30. CANONICAL `People's Self-Help Housing Corporation, San Luis Obispo`
     alt-with-chapters: `PEOPLE'S SELF-HELP HOUSING CORPORATION`
     chapter child(ren): ["PEOPLES' SELF -HELP HOUSING CORPORATION (SANTA BARBARA)"]

31. CANONICAL `POWER (People Organized to Win Employment Rights)`
     alt-with-chapters: `POWER`
     chapter child(ren): ['POWER CALIFORNIA']

32. CANONICAL `PRINTING INDUSTRIES OF CALIFORNIA`
     alt-with-chapters: `Printing Industries`
     chapter child(ren): ['Printing Industries of CA']

33. CANONICAL `Project Sentinel HUD Housing Counseling Programs`
     alt-with-chapters: `PROJECT SENTINEL`
     chapter child(ren): ['Project Sentinel (Palo Alto)']

34. CANONICAL `Screen Actors Guild - American Federation of Television and Radio Artists`
     alt-with-chapters: `SCREEN ACTORS GUILD`
     chapter child(ren): ['California Screen Actors Guild']

35. CANONICAL `Service Employees International Union`
     alt-with-chapters: `Service Employees`
     chapter child(ren): ['Service Employees of CA', 'Service Employees of California']

36. CANONICAL `Service Employees International Union`
     alt-with-chapters: `SERVICE EMPLOYEES INTERNATIONAL`
     chapter child(ren): ['Service Employees International Local 1000', 'Service Employees International Local 535', 'Service Employees International Local 535 434B']

37. CANONICAL `Service Employees International Union`
     alt-with-chapters: `Service Employees International Union AFL-CIO, CLC`
     chapter child(ren): ['Service Employees International Union AFL-CIO, CLC, California']

38. CANONICAL `Service Employees International Union`
     alt-with-chapters: `Service Employees Union`
     chapter child(ren): ['Service Employees Union of California', 'California Service Employees Union']

39. CANONICAL `Service Employees International Union`
     alt-with-chapters: `SEIU (Service Employees International Union)`
     chapter child(ren): ['SEIU (Service Employees International Union), Local 660 and Local 1877']

40. CANONICAL `Service Employees International Union`
     alt-with-chapters: `Service Employees International Union - AFL-CIO`
     chapter child(ren): ['Service Employees International Union - AFL-CIO, Local 660']

41. CANONICAL `Service Employees International Union`
     alt-with-chapters: `Service Employees Union International`
     chapter child(ren): ['Service Employees Union International, Local 1000']

42. CANONICAL `Service Employees International Union`
     alt-with-chapters: `Service Employees International Union plus`
     chapter child(ren): ['Service Employees International Union plus Local 2028']

43. CANONICAL `Service Employees International Union`
     alt-with-chapters: `Service Employees International Union, AFL-CIO, CA State Council and`
     chapter child(ren): ['Service Employees International Union, AFL-CIO, CA State Council and Local 660']

44. CANONICAL `Service Employees International Union`
     alt-with-chapters: `State Employees International Union`
     chapter child(ren): ['State Employees International Union Local 1000']

45. CANONICAL `Service Employees International Union`
     alt-with-chapters: `Service Employees International Union, AFL-CIO, CLC (SEIU)`
     chapter child(ren): ['Service Employees International Union (SEIU) : SEIU Local 434B', 'SEIU Bart Chapter, Local 1021', 'Service Employees International Union (SEIU) Local 1000, which represents the attorney and non-attorney employees of the State Bar', 'Service Employees International Union (SEIU), Local 1000, represents 95,000 public employees statewide and', 'Service Employees International Union (SEIU), Local 1000, represents 96,000 public employees statewide and', 'Services Employees International Union (SEIU): SEIU Local 434B']

46. CANONICAL `Technet Technology Network`
     alt-with-chapters: `TECHNET`
     chapter child(ren): ['Technet California']

47. CANONICAL `Transport Workers Union of America, California State Conference`
     alt-with-chapters: `Transportation Workers Union of America`
     chapter child(ren): ['Transportation Workers Union of America, Local 250-A, AFL-CIO']

48. CANONICAL `Transport Workers Union of America, California State Conference`
     alt-with-chapters: `Transportation Workers`
     chapter child(ren): ['Transportation Workers Local 105']

49. CANONICAL `UNITED AUTO WORKERS`
     alt-with-chapters: `United Automobile Workers`
     chapter child(ren): ['United Automobile Workers, Local 4123', 'United Automobile Workers, Local 5810', 'United Automobile Workers Local 2865', 'United Automobile Workers, Local 2864']

50. CANONICAL `UNITED AUTO WORKERS`
     alt-with-chapters: `United Automobile, Aerospace and Agricultural Implement Workers of America`
     chapter child(ren): ['United Automobile, Aerospace and Agricultural Implement Workers of America, Local 5810']

51. CANONICAL `UNITED AUTO WORKERS`
     alt-with-chapters: `United Automotive Workers`
     chapter child(ren): ['United Automotive Workers, Local 2865']

52. CANONICAL `UNITED AUTO WORKERS`
     alt-with-chapters: `United Autoworkers`
     chapter child(ren): ['United Autoworkers, Local 2865 and Local 4123', 'United Autoworkers Local 4811']

53. CANONICAL `United Food and Commercial Workers`
     alt-with-chapters: `Commercial Workers Union`
     chapter child(ren): ['Commercial Workers Union, Local 78 (Alta Loma)', 'Commercial Workers Union, Local 58 (La Verne)', 'Commercial Workers Union, Local 47 (West Covina)', 'Commercial Workers Union, Local 350 (Temple City)']

54. CANONICAL `United Long-Term Care Workers Union`
     alt-with-chapters: `SEIU-ULTCW`
     chapter child(ren): ['SEIU-ULTCW Local 6434']

55. CANONICAL `United Long-Term Care Workers Union`
     alt-with-chapters: `United Long Term Care Workers Union (SEIU)`
     chapter child(ren): ['United Long Term Care Workers Union (SEIU) Local 6434']

56. CANONICAL `Workers Union`
     alt-with-chapters: `Workers Union of America`
     chapter child(ren): ['Workers Union of America, Local 132', 'Workers Union of America, Local 250-A, AFL-CIO']

