# oa-nz

A dashboard to display rates of open access for publications authored by researchers affiliated with NZ universities and Australian 'group of 8' universities, for comparison. 

## Background

The Ministry of Business, Innovation, and Employment (MBIE) released an [Open Research Policy](https://www.mbie.govt.nz/science-and-technology/science-and-innovation/agencies-policies-and-budget-initiatives/open-research-policy/) in November 2022 which:

> ...requires that all peer-reviewed publications arising from research funded through research investment processes administered by MBIE be made available with Open Access (free of charge, online access for any person) through one of the approved pathways.

Universities NZ released a [Pan-University Statement on Open Access](https://www.universitiesnz.ac.nz/sites/default/files/uni-nz/documents/Open%20Access%20Statement.pdf) which includes a goal to:

> Increase open access across our university repositories from 48% of published research (current)
to 70% by 2025.

This dashboard is an attempt to monitor rates of open access across New Zealand universities.

## Data

Data is parsed from the [Open Alex API](https://docs.openalex.org/). Publications are included if they have an author affiliated with an NZ or Australian 'group of 8' university, based on the university's Research Organisation Registry (ROR) code. Publications include peer reviewed articles, books, and book chapters, but exclude paratext (material about the journal such as covers, editorial board, or issue information) and retracted items.

Citation data was collected manually from OpenAlex for each institution for articles, books, book chapters, and reviews.

## Licence

All code in this repository is licensed under the MIT license.

[Open Access logo image](https://en.m.wikipedia.org/wiki/File:Open_Access_PLoS.svg) by 'art designer at PLoS' is licensed under the [Creative Commons Attribution-Share Alike 3.0 Unported](https://creativecommons.org/licenses/by-sa/3.0/deed.en) license. No changes were made.
