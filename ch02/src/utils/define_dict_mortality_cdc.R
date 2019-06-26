########################################
# create dictionary CDC mortality files
# author: sebastian daza
########################################


# list to save data
dict = list()

# 2000
dict[['2000/MORT00US.PT2']] = list(
  start = c(19,20,59,60,62,64,71,82,115,119,121,124,126,157),
  end   = c(19,20,59,61,62,66,72,82,118,120,123,125,128,158),
  names = c("rec_type","rec_status","sex",
            "race1","race2","age", "age_rec","race3",
            "year", "state_occ_fips","county_occ_fips","state_resid_fips",
            "county_resid_fips", "causes39")
)


# 2001
dict[['2001/Mort01us.dat']] = list(
  start = c(19,20,59,60,62,64,71,82,115,119,121,124,126,157),
  end   = c(19,20,59,61,62,66,72,82,118,120,123,125,128,158),
  names = c("rec_type","rec_status","sex",
            "race1","race2", "age", "age_rec","race3",
            "year", "state_occ_fips", "county_occ_fips","state_resid_fips",
            "county_resid_fips", "causes39")
)

# 2002
dict[['2002/Mort02us.dat']] = list(
 start = c(19,20,59,60,62,64,71,82,115,119,121,124,126,157),
 end   = c(19,20,59,61,62,66,72,82,118,120,123,125,128,158),
 names = c("rec_type","rec_status","sex","race1",
           "race2","age", "age_rec","race3", "year",
           "state_occ_fips","county_occ_fips","state_resid_fips",
           "county_resid_fips","causes39")
)

# 2003
dict[['2003/Mult03us.dat']] = list(
 start = c(19,20,21,23,29,35,69,70,79,102,160,445,449,488),
 end   = c(19,20,22,25,30,37,69,73,80,105,161,446,449,488),
 names = c("rec_type","rec_status","state_occ_fips","county_occ_fips",
            "state_resid_fips","county_resid_fips",
            "sex","age","age_rec","year",
            "causes39","race1","race2","race3")
)

# 2004
dict[['2004/Mort04us.dat']] = list(
 start = c(19,20,21,23,29,35,69,70,79,102,160,445,449,488),
 end   = c(19,20,22,25,30,37,69,73,80,105,161,446,449,488),
 names = c("rec_type","rec_status","state_occ_fips","county_occ_fips",
            "state_resid_fips","county_resid_fips",
            "sex","age","age_rec","year","causes39",
            "race1","race2","race3")
 )

# 2005
dict[['2005/Mort05usp2.dat']] = list(
 start = c(19,20,21,23,29,35,69,70,79,102,160,445,449,488),
 end   = c(19,20,22,25,30,37,69,73,80,105,161,446,449,488),
 names = c("rec_type","rec_status","state_occ_fips","county_occ_fips",
            "state_resid_fips","county_resid_fips",
            "sex","age","age_rec","year","causes39",
            "race1","race2","race3")
)

# 2006
dict[['2006/MULT2006.USPART2']] = list(
 start = c(19,20,21,23,29,35,69,70,79,102,160,445,449,488),
 end   = c(19,20,22,25,30,37,69,73,80,105,161,446,449,488),
 names = c("rec_type","rec_status","state_occ_fips","county_occ_fips",
            "state_resid_fips","county_resid_fips","sex",
            "age","age_rec","year","causes39",
            "race1","race2","race3")
)

# 2007
dict[['2007/MULT2007.USPART2']] = list(
 start = c(19,20,21,23,29,35,69,70,79,102,160,445,449,488),
 end   = c(19,20,22,25,30,37,69,73,80,105,161,446,449,488),
 names = c("rec_type","rec_status","state_occ_fips","county_occ_fips",
           "state_resid_fips","county_resid_fips",
           "sex","age","age_rec","year",
           "causes39", "race1","race2","race3")
)

# 2008
dict[['2008/MULT2008.USPART2']] = list(
 start = c(19,20,21,23,29,35,69,70,79,102,160,445,449,488),
 end   = c(19,20,22,25,30,37,69,73,80,105,161,446,449,488),
 names = c("rec_type","rec_status","state_occ_fips","county_occ_fips",
            "state_resid_fips","county_resid_fips",
            "sex","age","age_rec","year","causes39",
            "race1","race2","race3")
)

# 2009
dict[['2009/MULT2009.USPART2']] = list(
 start = c(19,20,21,23,29,35,69,70,79,102,160,445,449,488),
 end   = c(19,20,22,25,30,37,69,73,80,105,161,446,449,488),
 names = c("rec_type","rec_status","state_occ_fips","county_occ_fips",
            "state_resid_fips","county_resid_fips",
            "sex","age","age_rec","year","causes39",
            "race1","race2","race3")
)

# 2010
dict[['2010/MULT2010.USPART2.EXACTDOD']] = list(
 start = c(19,20,21,23,29,35,69,70,79,102,160,445,449,488),
 end   = c(19,20,22,25,30,37,69,73,80,105,161,446,449,488),
 names = c("rec_type","rec_status","state_occ_fips","county_occ_fips",
            "state_resid_fips","county_resid_fips","sex",
            "age","age_rec","year","causes39",
            "race1","race2","race3")
)

# 2011
dict[['2011/MULT2011.USPART2']] = list(
 start = c(19,20,21,23,29,35,69,70,79,102,160,445,449,488),
 end   = c(19,20,22,25,30,37,69,73,80,105,161,446,449,488),
 names = c("rec_type","rec_status","state_occ_fips","county_occ_fips",
            "state_resid_fips","county_resid_fips","sex",
            "age","age_rec","year","causes39",
            "race1","race2","race3")
)

# 2012
dict[['2012/MULT2012.USPART2']] = list(
 start = c(19,20,21,23,29,35,69,70,79,102,160,445,449,488),
 end   = c(19,20,22,25,30,37,69,73,80,105,161,446,449,488),
 names = c("rec_type","rec_status","state_occ_fips","county_occ_fips",
            "state_resid_fips","county_resid_fips","sex",
            "age","age_rec","year","causes39",
            "race1","race2","race3")
)

# 2013
dict[['2013/MULT2013.USPART2']] = list(
 start = c(19,20,21,23,29,35,69,70,79,102,160,445,449,488),
 end   = c(19,20,22,25,30,37,69,73,80,105,161,446,449,488),
 names = c("rec_type","rec_status","state_occ_fips","county_occ_fips",
            "state_resid_fips","county_resid_fips","sex",
            "age","age_rec","year","causes39",
            "race1","race2","race3")
)

# 2014
dict[['2014/MULT2014.USPART2']] = list(
 start = c(19,20,21,23,29,35,69,70,79,102,160,445,449,488),
 end   = c(19,20,22,25,30,37,69,73,80,105,161,446,449,488),
 names = c("rec_type","rec_status","state_occ_fips","county_occ_fips",
            "state_resid_fips","county_resid_fips","sex",
            "age","age_rec","year","causes39",
            "race1","race2","race3")
)

# save dictionary
saveRDS(dict, 'output/dict_mortality_files.rds')
