library(dtplyr)
library(dplyr)
library(data.table)

bety <- src_postgres(dbname = 'bety',
                     user = 'bety',
                     password = 'bety')

dat <- readRDS('../extdata/prospect_results.rds')

my_species <- distinct(dat, SpeciesCode)

ed_modeltype <- 1

used_pfts <- c('temperate.Early_Hardwood',
               'temperate.North_Mid_Hardwood',
               'temperate.Late_Hardwood',
               'temperate.Northern_Pine',
               'temperate.Southern_Pine',
               'temperate.Mid_conifer',
               'temperate.Late_Conifer')

#pfts <- tbl(bety, 'pfts') %>%
    #filter(modeltype_id == ed_modeltype) %>%
    #select(pft.id = id, pft.name = name) %>%
    #filter(pft.name %like% '%temperate%') %>%
    #collect()

ed_pft_species <- tbl(bety, 'species') %>%
    select(species.id = id, SpeciesCode = AcceptedSymbol) %>%
    inner_join(tbl(bety, 'pfts_species') %>%
               select(species.id = specie_id, pft.id = pft_id)) %>%
    inner_join(tbl(bety, 'pfts') %>%
               filter(modeltype_id == ed_modeltype,
                      name %in% used_pfts) %>%
               select(pft.id = id, pft.name = name)) %>%
    filter(SpeciesCode != '') %>%
    select(SpeciesCode, pft.name) %>%
    collect(n = Inf) %>%
    setDT() %>%
    setkey(SpeciesCode)

merged_species <- inner_join(ed_pft_species, my_species)

dat_pft <- inner_join(dat, merged_species)
saveRDS(dat_pft, '../extdata/dat_pft.rds')
