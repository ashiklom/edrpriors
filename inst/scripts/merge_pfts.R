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
    select(SpeciesCode, pft.name, bety_species_id = species.id) %>%
    collect(n = Inf) %>%
    setDT() %>%
    setkey(SpeciesCode)

merged_species <- inner_join(ed_pft_species, my_species)

tbl(bety, 'variables') %>%
    #filter(name %like% '%LMA%')
    filter(name %like% '%SLA%')

bety_traits <- tbl(bety, 'traits') %>%
    filter(specie_id %in% merged_species$bety_species_id,
           variable_id %in% c(15, 254)) %>%
    select(bety_species_id = specie_id, variable_id,
           value = mean, entity_id) %>%
    collect %>%
    mutate(variable_name = recode(variable_id, `15` = 'SLA', `254` = 'LMA')) %>%
    mutate(bety_lma = case_when(.$variable_name == 'SLA' ~ 1/.$value,
                                 .$variable_name == 'LMA' ~ .$value,
                                 TRUE ~ NA_real_),
           leaf_mass_per_area = udunits2::ud.convert(bety_lma, 'kg m-2', 'g m-2')) %>%
    filter(bety_lma < 1) %>%
    select(bety_species_id, leaf_mass_per_area) %>%
    mutate(FullName = paste0('bety_trait_', row_number()),
           Project = 'bety_trait') %>%
    left_join(merged_species) %>%
    setDT()

prospect5_results <- inner_join(dat, merged_species) %>%
    full_join(bety_traits) %>%
    mutate(leaf_mass_per_area = if_else(is.nan(leaf_mass_per_area), 
                                        NA_real_, leaf_mass_per_area))
    
save(prospect5_results, file = '../../data/prospect5_results.RData')
