# Search methods and results

Search date: **6 November 2026**. 
PubMed/MEDLINE, Scopus, Web of Science Core Collection and CENTRAL were searched without date, language or outcome restrictions. Search filters were adapted and tested against 25 known reports.

## Records identified

| Source | Records before deduplication |
|---|---:|
| PubMed | 3,447 |
| Scopus | 4,802 |
| Web of Science | 4,110 |
| CENTRAL | 3,006 |
| Citation searching | 1,373 |
| ClinicalTrials.gov | 503 |
| WHO ICTRP | 535 |
| Other review sources | 3 |
| **Total** | **17,779** |

PRISMA counts by identification route are provided in `../screening/prisma_counts.csv`.

Source occurrences and retained records are linked in `../screening/source_provenance.csv`. The README defines duplicate-copy counts and screening subsets.

## Search coverage 

Known-report retrieval was 24/24 indexed controls in PubMed and 25/25 in Scopus and Web of Science. CENTRAL retained 22 distinct DOI-located control reports; three reports were not located by DOI/title. The 25 control reports are listed in `search_controls.csv`.

## PubMed

```text
(("Vitamin K"[MeSH Terms] OR "vitamin K"[Title/Abstract] OR "vitamin K1"[Title/Abstract] OR "vitamin K2"[Title/Abstract] OR "vitamin K3"[Title/Abstract] OR "vitamin K4"[Title/Abstract] OR "vitamin K 1"[Title/Abstract] OR "vitamin K 2"[Title/Abstract] OR "vitamin K 3"[Title/Abstract] OR "vitamin K 4"[Title/Abstract] OR phylloquinon*[Title/Abstract] OR phytonadione[Title/Abstract] OR phytomenadione[Title/Abstract] OR menaquinon*[Title/Abstract] OR menatetrenone[Title/Abstract] OR menadione[Title/Abstract] OR menadiol[Title/Abstract] OR "MK-4"[Title/Abstract] OR "MK-7"[Title/Abstract] OR "MK 4"[Title/Abstract] OR "MK 7"[Title/Abstract] OR MK4[Title/Abstract] OR MK7[Title/Abstract]) AND (randomized controlled trial[Publication Type] OR controlled clinical trial[Publication Type] OR randomized[Title/Abstract] OR placebo[Title/Abstract] OR clinical trials as topic[MeSH Terms:noexp] OR randomly[Title/Abstract] OR trial[Title] OR RCT[Title/Abstract] OR supplement*[Title])) NOT (animals[MeSH Terms] NOT humans[MeSH Terms])
```

## Scopus

```text
TITLE-ABS-KEY("vitamin K" OR "vitamin K1" OR "vitamin K2" OR "vitamin K3" OR "vitamin K4" OR "vitamin K 1" OR "vitamin K 2" OR "vitamin K 3" OR "vitamin K 4" OR phylloquinon* OR phytonadione OR phytomenadione OR menaquinon* OR menatetrenone OR menadione OR menadiol OR "MK-4" OR "MK-7" OR "MK 4" OR "MK 7" OR MK4 OR MK7) AND (TITLE({clinical-trial} OR {controlled-trial} OR randomi* OR randomly OR (random W/4 (allocat* OR distribut* OR assign*)) OR placebo* OR rct) OR ABS({clinical-trial} OR {controlled-trial} OR randomi* OR randomly OR (random W/4 (allocat* OR distribut* OR assign*)) OR placebo* OR rct) OR TITLE(trial OR supplement*))
```

## Web of Science Core Collection

```text
TS=("vitamin K" OR "vitamin K1" OR "vitamin K2" OR "vitamin K3" OR "vitamin K4" OR "vitamin K 1" OR "vitamin K 2" OR "vitamin K 3" OR "vitamin K 4" OR phylloquinon* OR phytonadione OR phytomenadione OR menaquinon* OR menatetrenone OR menadione OR menadiol OR "MK-4" OR "MK-7" OR "MK 4" OR "MK 7" OR MK4 OR MK7) AND (TS=(randomised OR randomized OR randomisation OR randomization OR placebo* OR (random* AND (allocat* OR assign*)) OR (blind* AND (single OR double OR treble OR triple))) OR TS=(RCT) OR TI=(supplement*))
```

## CENTRAL (Trials)

```text
#1 MeSH descriptor: [Vitamin K] explode all trees
#2 ("vitamin K" OR "vitamin K1" OR "vitamin K2" OR "vitamin K3" OR "vitamin K4" OR "vitamin K 1" OR "vitamin K 2" OR "vitamin K 3" OR "vitamin K 4" OR phylloquinon* OR phytonadione OR phytomenadione OR menaquinon* OR menatetrenone OR menadione OR menadiol OR "MK-4" OR "MK-7" OR "MK 4" OR "MK 7" OR MK4 OR MK7)
#3 #1 OR #2
```

## Citation and additional searching

A targeted backward-citation check on **14 November 2026** added one report: Vermeer and Vik (2020), [DOI 10.15761/VDT.1000179](https://doi.org/10.15761/VDT.1000179), cited as reference 18 in Bittner et al. (2026). The record was already present in the authors’ Zotero library but had not been included in the screening accounting. It is now linked to `Bittner_2026` (NCT02404519): matching 243 participants, allocation 121/122, sex composition, eligibility, MK-7 180 µg/day for 12 months, and adherence 96.3%/97.5%, together with Bittner’s explicit methods citation, establish the linkage. This adds one source occurrence and one included report, with no additional trial or participants. Database searches remain dated 6 November 2026. The review now contains **74 reports from 26 trials**. The existing selected effects and outcome-specific participant counts are unchanged.

Backward and forward citation searching used Europe PMC for 24 PMID-indexed control reports and three related supplementation reviews (PMIDs 40054729, 38282652 and 29968548). OpenAlex supplied citation links for the Adeli report (DOI 10.34172/PS.2022.30) and the Jamka review (DOI 10.20883/medical.402). Three separately imported review records added no unique records beyond the database searches.

ClinicalTrials.gov was searched in the intervention field:

```text
"vitamin K" OR "vitamin K1" OR "vitamin K2" OR phylloquinone OR menaquinone OR menatetrenone OR phytonadione OR phytomenadione OR menadione OR "MK-7" OR "MK-4"
```

WHO ICTRP was searched with default synonyms:

```text
"vitamin K" OR phylloquinone OR menaquinone OR menatetrenone OR phytonadione OR phytomenadione OR menadione
```

WHO displayed 745 underlying records grouped into 535 trial entries; its CSV exported the 535 grouped rows, which are the imported count. Citation coverage is limited to the links indexed by these services. One of the Adeli report's 40 referenced-work identifiers was unavailable; incomplete citations remain in the retained records where identity could not be established. 

Final screening decisions are in `../screening/records.csv`. Original exports are retained by the review authors; record-level source provenance is provided in the screening files.
