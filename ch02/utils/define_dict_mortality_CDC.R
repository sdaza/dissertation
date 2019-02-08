########################################
# create dictionary CDC mortality files
# author: sebastian daza
########################################


library(here)


# list to save data
dic = list()

# 2000

dic[['2000/MORT00US.PT2']] = list(
  start = c(19,20,52,54,59,60,62,64,71,77,82,115,119,121,124,126,139,142,157),
  end = c(19,20,53,54,59,61,62,66,72,77,82,118,120,123,125,128,139,145,158),
  names = c("rec_type","rec_status", "education","education_rec","sex",
             "race1","race2","age", "age_rec", "martial_status","race3",
             "year", "state_occ_fips","county_occ_fips","state_resid_fips",
             "county_resid_fips","manner", "icd", "causes39")
         )


# 2001

dic[['2001/Mort01us.dat']] = list(
  start = c(19,20,52,54,59,60,62,64,71,77,82,115,119,121,124,126,139,157),
  end = c(19,20,53,54,59,61,62,66,72,77,82,118,120,123,125,128,139,158),
  names = c("rec_type","rec_status","education","education_rec","sex","race1","race2", "age", "age_rec", "martial_status","race3", "year", "state_occ_fips", "county_occ_fips","state_resid_fips","county_resid_fips","manner", "causes39")
         )

# 2002

dic[['2002/Mort02us.dat']] = list(
 start = c(19,20,52,54,59,60,62,64,71,77,82,115,119,121,124,126,139,157),
 end = c(19,20,53,54,59,61,62,66,72,77,82,118,120,123,125,128,139,158),
 names = c("rec_type","rec_status", "education","education_rec","sex","race1",
            "race2","age", "age_rec", "martial_status","race3", "year",
            "state_occ_fips","county_occ_fips","state_resid_fips",
            "county_resid_fips","manner", "causes39")
        )

# 2003

dic[['2003/Mult03us.dat']] = list(
 start = c(19,20,21,23,29,35,61,63,69,70,79,84,102,107,160,445,449,488),
 end = c(19,20,22,25,30,37,62,63,69,73,80,84,105,107,161,446,449,488),
 names = c("rec_type","rec_status","state_occ_fips","county_occ_fips",
            "state_resid_fips","county_resid_fips","education","edu03",
            "sex","age","age_rec","martial_status","year","manner",
            "causes39","race1","race2","race3")
        )

# 2004

dic[['2004/Mort04us.dat']] = list(
 start = c(19,20,21,23,29,35,61,63,69,70,79,84,102,107,160,445,449,488),
 end = c(19,20,22,25,30,37,62,63,69,73,80,84,105,107,161,446,449,488),
 names = c("rec_type","rec_status","state_occ_fips","county_occ_fips",
            "state_resid_fips","county_resid_fips","education","edu03",
            "sex","age","age_rec","martial_status","year","manner","causes39",
            "race1","race2","race3")
        )

# 2005

dic[['2005/Mort05usp2.dat']] = list(
 start = c(19,20,21,23,29,35,61,63,69,70,79,84,102,107,160,445,449,488),
 end = c(19,20,22,25,30,37,62,63,69,73,80,84,105,107,161,446,449,488),
 names = c("rec_type","rec_status","state_occ_fips","county_occ_fips",
            "state_resid_fips","county_resid_fips","education","edu03",
            "sex","age","age_rec","martial_status","year","manner","causes39",
            "race1","race2","race3")
        )

# 2006

dic[['2006/MULT2006.USPART2']] = list(
 start = c(19,20,21,23,29,35,61,63,69,70,79,84,102,107,160,445,449,488),
 end = c(19,20,22,25,30,37,62,63,69,73,80,84,105,107,161,446,449,488),
 names = c("rec_type","rec_status","state_occ_fips","county_occ_fips",
            "state_resid_fips","county_resid_fips","education","edu03","sex",
            "age","age_rec","martial_status","year","manner","causes39",
            "race1","race2","race3")
        )

# 2007

dic[['2007/MULT2007.USPART2']] = list(
 start = c(19,20,21,23,29,35,61,63,69,70,79,84,102,107,146,160,445,449,488),
 end = c(19,20,22,25,30,37,62,63,69,73,80,84,105,107,149,161,446,449,488),
 names = c("rec_type","rec_status","state_occ_fips","county_occ_fips",
            "state_resid_fips","county_resid_fips","education","edu03",
            "sex","age","age_rec","martial_status","year","manner","icd", "causes39", "race1","race2","race3")
        )

# 2008

dic[['2008/MULT2008.USPART2']] = list(
 start = c(19,20,21,23,29,35,61,63,69,70,79,84,102,107,160,445,449,488),
 end = c(19,20,22,25,30,37,62,63,69,73,80,84,105,107,161,446,449,488),
 names = c("rec_type","rec_status","state_occ_fips","county_occ_fips",
            "state_resid_fips","county_resid_fips","education","edu03",
            "sex","age","age_rec","martial_status","year","manner","causes39",
            "race1","race2","race3")
        )

# 2009

dic[['2009/MULT2009.USPART2']] = list(
 start = c(19,20,21,23,29,35,61,63,69,70,79,84,102,107,160,445,449,488),
 end = c(19,20,22,25,30,37,62,63,69,73,80,84,105,107,161,446,449,488),
 names = c("rec_type","rec_status","state_occ_fips","county_occ_fips",
            "state_resid_fips","county_resid_fips","education","edu03",
            "sex","age","age_rec","martial_status","year","manner","causes39",
            "race1","race2","race3")
        )

# 2010

dic[['2010/MULT2010.USPART2.EXACTDOD']] = list(
 start = c(19,20,21,23,29,35,61,63,69,70,79,84,102,107,160,445,449,488),
 end = c(19,20,22,25,30,37,62,63,69,73,80,84,105,107,161,446,449,488),
 names = c("rec_type","rec_status","state_occ_fips","county_occ_fips",
            "state_resid_fips","county_resid_fips","education","edu03","sex",
            "age","age_rec","martial_status","year","manner","causes39",
            "race1","race2","race3")
        )

# 2011

dic[['2011/MULT2011.USPART2']] = list(
 start = c(19,20,21,23,29,35,61,63,69,70,79,84,102,107,160,445,449,488),
 end = c(19,20,22,25,30,37,62,63,69,73,80,84,105,107,161,446,449,488),
 names = c("rec_type","rec_status","state_occ_fips","county_occ_fips",
            "state_resid_fips","county_resid_fips","education","edu03","sex",
            "age","age_rec","martial_status","year","manner","causes39",
            "race1","race2","race3")
        )

# 2012

dic[['2012/MULT2012.USPART2']] = list(
 start = c(19,20,21,23,29,35,61,63,69,70,79,84,102,107,160,445,449,488),
 end = c(19,20,22,25,30,37,62,63,69,73,80,84,105,107,161,446,449,488),
 names = c("rec_type","rec_status","state_occ_fips","county_occ_fips",
            "state_resid_fips","county_resid_fips","education","edu03","sex",
            "age","age_rec","martial_status","year","manner","causes39",
            "race1","race2","race3")
        )

# 2013

dic[['2013/MULT2013.USPART2']] = list(
 start = c(19,20,21,23,29,35,61,63,69,70,79,84,102,107,160,445,449,488),
 end = c(19,20,22,25,30,37,62,63,69,73,80,84,105,107,161,446,449,488),
 names = c("rec_type","rec_status","state_occ_fips","county_occ_fips",
            "state_resid_fips","county_resid_fips","education","edu03","sex",
            "age","age_rec","martial_status","year","manner","causes39",
            "race1","race2","race3")
        )

# 2014

dic[['2014/MULT2014.USPART2']] = list(
 start = c(19,20,21,23,29,35,61,63,69,70,79,84,102,107,160,445,449,488),
 end = c(19,20,22,25,30,37,62,63,69,73,80,84,105,107,161,446,449,488),
 names = c("rec_type","rec_status","state_occ_fips","county_occ_fips",
            "state_resid_fips","county_resid_fips","education","edu03","sex",
            "age","age_rec","martial_status","year","manner","causes39",
            "race1","race2","race3")
        )

# save dict
saveRDS(dic, 'cdc_mortality/output/dict_mortality_files.rds')
