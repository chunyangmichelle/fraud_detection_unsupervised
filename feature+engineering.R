

data = readRDS("my_data.rds")

data2=data%>%
  mutate(lotarea=LTFRONT * LTDEPTH)%>%
  mutate(bldarea=BLDFRONT * BLDDEPTH)%>%
  mutate(bldvol=bldarea * STORIES)

data_45_None = data2 %>%
  mutate(fV_la_None= FULLVAL/lotarea) %>%
  mutate(fV_ba_None= FULLVAL/bldarea) %>%
  mutate(fV_bv_None= FULLVAL/bldvol) %>%
  mutate(vl_la_None= AVLAND/lotarea) %>%
  mutate(vl_ba_None= AVLAND/bldarea) %>%
  mutate(vl_bv_None= AVLAND/bldvol) %>%
  mutate(vt_la_None= AVTOT/lotarea) %>%
  mutate(vt_ba_None= AVTOT/bldarea) %>%
  mutate(vt_bv_None= AVTOT/bldvol)



data_45_ZIP3 = data_45_None %>%
  group_by(ZIP3) %>%
  mutate(fV_la_ZIP3= mean(FULLVAL/lotarea)) %>%
  mutate(fV_ba_ZIP3= mean(FULLVAL/bldarea)) %>%
  mutate(fV_bv_ZIP3= mean(FULLVAL/bldvol)) %>%
  mutate(vl_la_ZIP3= mean(AVLAND/lotarea)) %>%
  mutate(vl_ba_ZIP3= mean(AVLAND/bldarea)) %>%
  mutate(vl_bv_ZIP3= mean(AVLAND/bldvol)) %>%
  mutate(vt_la_ZIP3= mean(AVTOT/lotarea)) %>%
  mutate(vt_ba_ZIP3= mean(AVTOT/bldarea)) %>%
  mutate(vt_bv_ZIP3= mean(AVTOT/bldvol))

data_45_ZIP5 = data_45_ZIP3 %>%
  group_by(ZIP) %>%
  mutate(fV_la_ZIP5= mean(FULLVAL/lotarea)) %>%
  mutate(fV_ba_ZIP5= mean(FULLVAL/bldarea)) %>%
  mutate(fV_bv_ZIP5= mean(FULLVAL/bldvol)) %>%
  mutate(vl_la_ZIP5= mean(AVLAND/lotarea)) %>%
  mutate(vl_ba_ZIP5= mean(AVLAND/bldarea)) %>%
  mutate(vl_bv_ZIP5= mean(AVLAND/bldvol)) %>%
  mutate(vt_la_ZIP5= mean(AVTOT/lotarea)) %>%
  mutate(vt_ba_ZIP5= mean(AVTOT/bldarea)) %>%
  mutate(vt_bv_ZIP5= mean(AVTOT/bldvol))


data_45_TAX = data_45_ZIP5 %>%
  group_by(TAXCLASS) %>%
  mutate(fV_la_TAX= mean(FULLVAL/lotarea)) %>%
  mutate(fV_ba_TAX= mean(FULLVAL/bldarea)) %>%
  mutate(fV_bv_TAX= mean(FULLVAL/bldvol)) %>%
  mutate(vl_la_TAX= mean(AVLAND/lotarea)) %>%
  mutate(vl_ba_TAX= mean(AVLAND/bldarea)) %>%
  mutate(vl_bv_TAX= mean(AVLAND/bldvol)) %>%
  mutate(vt_la_TAX= mean(AVTOT/lotarea)) %>%
  mutate(vt_ba_TAX= mean(AVTOT/bldarea)) %>%
  mutate(vt_bv_TAX= mean(AVTOT/bldvol))

data_45_BORO = data_45_TAX %>%
  group_by(BORO) %>%
  mutate(fV_la_BORO= mean(FULLVAL/lotarea)) %>%
  mutate(fV_ba_BORO= mean(FULLVAL/bldarea)) %>%
  mutate(fV_bv_BORO= mean(FULLVAL/bldvol)) %>%
  mutate(vl_la_BORO= mean(AVLAND/lotarea)) %>%
  mutate(vl_ba_BORO= mean(AVLAND/bldarea)) %>%
  mutate(vl_bv_BORO= mean(AVLAND/bldvol)) %>%
  mutate(vt_la_BORO= mean(AVTOT/lotarea)) %>%
  mutate(vt_ba_BORO= mean(AVTOT/bldarea)) %>%
  mutate(vt_bv_BORO= mean(AVTOT/bldvol))

finaldata = data_45_BORO[, 18:62]

saveRDS(finaldata, file = "finaldata.rds")
