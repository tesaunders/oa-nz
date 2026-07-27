# NZ/AU Open Access Dashboard

A dashboard to display rates of open access for publications authored by researchers affiliated with NZ universities and Australian 'group of 8' universities, for comparison. 

## Data

Data is retrieved from the [OpenAlex API](https://docs.openalex.org/). Publications are included if they have an author affiliated with an NZ or Australian 'group of 8' university. Publications include peer reviewed articles, books, and book chapters, but exclude paratext (material about the journal such as covers, editorial board, or issue information) and retracted items.

You will need a free [OpenAlex API key](https://developers.openalex.org/guides/authentication#getting-an-api-key) to run the code yourself. An API key can be set as an environment variable within R, otherwise you'll be restricted to the free limits which won't be enough to run the script. To set an API key:

```
Sys.getenv("YOUR_OPENALEX_API_KEY")
```

## Background

The Ministry of Business, Innovation, and Employment (MBIE) released an [Open Research Policy](https://www.mbie.govt.nz/science-and-technology/science-and-innovation/agencies-policies-and-budget-initiatives/open-research-policy/) in November 2022 which:

> ...requires that all peer-reviewed publications arising from research funded through research investment processes administered by MBIE be made available with Open Access (free of charge, online access for any person) through one of the approved pathways.

Universities NZ released a [Pan-University Statement on Open Access](https://www.universitiesnz.ac.nz/sites/default/files/uni-nz/documents/Open%20Access%20Statement.pdf) which includes a goal to:

> Increase open access across our university repositories from 48% of published research (current) to 70% by 2025.

This dashboard is an attempt to monitor rates of open access across New Zealand universities.

## Licence

All data is CC0 public domain. All code is licensed under the MIT license.

[Open Access logo image](https://en.m.wikipedia.org/wiki/File:Open_Access_PLoS.svg) by 'art designer at PLoS' is licensed under the [Creative Commons Attribution-Share Alike 3.0 Unported](https://creativecommons.org/licenses/by-sa/3.0/deed.en) license. No changes were made.