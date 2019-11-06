###################################
# individual income mobility paper
# psid well-being data
# author: sebastian daza
# version: 0.01
###################################


library(haven)
library(data.table)
library(hash)

source("ch03/src/utils.R")

# respondents to identify newborns
data = data.table(read_stata("ch03/data/psid/respondents_covs.dta"))
family = data.table(read_stata("ch03/data/psid/family_matrix.dta"))

# family dataset
setnames(family, names(family), tolower(names(family)))

# select parents
family = family[mx8 == 50]
family[, pid_parent := mx5 * 1000 + mx6]
family[, pid_kid := mx10 * 1000 + mx11]
setnames(family, "mx2", "year")
family = family[, .(pid_kid, pid_parent)]
family = unique(family)

# individuals
setnames(data, names(data), tolower(names(data)))
data[, pid := er30001 * 1000 + er30002]

anyDuplicated(data[, pid])

# demography variables
demographic_hash = hash(
                        "er32000" = "sex",
                        "er32014" = "birth_weight",
                        "er32015" = "marital_status_mother_at_birth",
                        "er32011" = "mother_born_year",
                        "er34410" = "wellbeing_sample"
                        )
renameColumns(data, demographic_hash)

# wellbeing flag
data[, wellbeing_sample := ifelse(wellbeing_sample %in% c(1, 2), 1, 0)]
table(data$wellbeing_sample)

# sequence of years
years = c(1968:1997, seq(1999, 2017, 2))

# family number
oldvars = c("v3", "v442", "v1102", "v1802", "v2402", "v3002", "v3402", "v3802", "v4302",
            "v5202", "v5702", "v6302", "v6902", "v7502", "v8202", "v8802", "v10002",
            "v11102", "v12502", "v13702", "v14802", "v16302", "v17702", "v19002",
            "v20302", "v21602", "er2002", "er5002", "er7002", "er10002", "er13002",
            "er17002", "er21002", "er25002", "er36002", "er42002", "er47302",
            "er53002", "er60002", "er66002")

fn_vars = paste0("fn", years)
renameColumns(data, hash(oldvars, fn_vars))

# sequence number
oldvars = c("er30021", "er30044", "er30068", "er30092", "er30118", "er30139", "er30161",
            "er30189", "er30218", "er30247", "er30284", "er30314", "er30344", "er30374",
            "er30400", "er30430", "er30464", "er30499", "er30536", "er30571", "er30607",
            "er30643", "er30690", "er30734", "er30807", "er33102", "er33202", "er33302",
            "er33402", "er33502", "er33602", "er33702", "er33802", "er33902", "er34002",
            "er34102", "er34202", "er34302", "er34502")

sn_vars = paste0("sn", years[-1])
renameColumns(data, hash(oldvars, sn_vars))

data[, sn1968 := NA]
sn_vars = c("sn1968", sn_vars)

# person number
setnames(data, "er30002", "pn")

# type of individual
oldvars = c("er30017", "er30040", "er30064", "er30088", "er30114", "er30135", "er30157",
            "er30185", "er30214", "er30243", "er30280", "er30310", "er30340", "er30370",
            "er30396", "er30426", "er30460", "er30495", "er30532", "er30567", "er30603",
            "er30639", "er30684", "er30728", "er30801", "er30862", "er33126", "er33282",
            "er33324", "er33436", "er33544", "er33635", "er33738", "er33846", "er33948",
            "er34043", "er34152", "er34266", "er34411", "er34648")

type_vars = paste0("type", years)
renameColumns(data, hash(oldvars, type_vars))

# relationship to head
oldvars = c("er30003", "er30022", "er30045", "er30069", "er30093", "er30119", "er30140",
            "er30162", "er30190", "er30219", "er30248", "er30285", "er30315", "er30345",
            "er30375", "er30401", "er30431", "er30465", "er30500", "er30537", "er30572",
            "er30608", "er30644", "er30691", "er30735", "er30808", "er33103", "er33203",
            "er33303", "er33403", "er33503", "er33603", "er33703", "er33803", "er33903",
            "er34003", "er34103", "er34203", "er34303", "er34503")

relationhead_vars = paste0("relationhead", years)
renameColumns(data, hash(oldvars, relationhead_vars))

# respondent
oldvars = c("v180", "v800", "v1489", "v2201", "v2827", "v3248", "v3670", "v4149", "v4700",
            "v5618", "v6165", "v6764", "v7397", "v8049", "v8673", "v9359", "v11006",
            "v12354", "v13607", "v14654", "v16128", "v17525", "v18856", "v20156",
            "v21462", "v23318", "er2013", "er5012", "er7012", "er10015", "er13016",
            "er17019", "er24073", "er27879", "er40869", "er46697", "er52097", "er57901",
            "er65081", "er71164")

respondent_vars = paste0("respondent", years)
renameColumns(data, hash(oldvars, respondent_vars))

# why non-response
oldvars = c("er30018", "er30041", "er30065", "er30089", "er30115", "er30136", "er30158",
            "er30186", "er30215", "er30244", "er30281", "er30311", "er30341", "er30371",
            "er30397", "er30427", "er30461", "er30496", "er30533", "er30568", "er30604",
            "er30640", "er30685", "er30729", "er30802", "er30863", "er33127", "er33283",
            "er33325", "er33437", "er33545", "er33636", "er33739", "er33847", "er33949",
            "er34044", "er34153", "er34267", "er34412", "er34649")

whynoresp_vars = paste0("whynoresp", years)
renameColumns(data, hash(oldvars, whynoresp_vars))

# is respondent
oldvars = c("er30031", "er30055", "er30079", "er30104", "er30128", "er30149", "er30170",
            "er30200", "er30229", "er30266", "er30292", "er30322", "er30352", "er30381",
            "er30410", "er30440", "er33511", "er33611", "er33711", "er33811", "er33911",
            "er34011", "er34111", "er34211", "er34312", "er34511")

syears = c(years[years > 1968 & years < 1985], years[years > 1998])
isrespondent_vars = paste0("isrespondent", syears)
renameColumns(data, hash(oldvars, isrespondent_vars))

isrespondent_vars = paste0("isrespondent", years)
fillMissingColumns(data, "isrespondent", isrespondent_vars)

# age
oldvars = c("er30004", "er30023", "er30046", "er30070", "er30094", "er30120", "er30141",
            "er30163", "er30191", "er30220", "er30249", "er30286", "er30316", "er30346",
            "er30376", "er30402", "er30432", "er30466", "er30501", "er30538", "er30573",
            "er30609", "er30645", "er30692", "er30736", "er30809", "er33104", "er33204",
            "er33304", "er33404", "er33504", "er33604", "er33704", "er33804", "er33904",
            "er34004", "er34104", "er34204", "er34305", "er34504")

age_vars = paste0("age", years)
renameColumns(data, hash(oldvars, age_vars))

# race
oldvars = c("v181", "v801", "v1490", "v2202", "v2828", "v3300", "v3720", "v4204", "v5096",
            "v5662", "v6209", "v6802", "v7447", "v8099", "v8723", "v9408", "v11055", "v11938",
            "v13565", "v14612", "v16086", "v17483", "v18814", "v20114", "v21420", "v23276",
            "er3944", "er6814", "er9060", "er11848", "er15928", "er19989", "er23426", "er27393",
            "er40565", "er46543", "er51904", "er57659", "er64810", "er70882")

headrace_vars = paste0("headrace", years)
renameColumns(data, hash(oldvars, headrace_vars))

oldvars = c("v12293", "v13500", "v14547", "v16021", "v17418", "v18749", "v20049", "v21355",
            "v23212", "er3883", "er6753", "er8999", "er11760", "er15836", "er19897",
            "er23334", "er27297", "er40472", "er46449", "er51810", "er57549", "er64671",
            "er70744")

syears = years[years > 1984]
wiferace_vars = paste0("wiferace", syears)
renameColumns(data, hash(oldvars, wiferace_vars))

wiferace_vars = paste0("wiferace", years)
fillMissingColumns(data, "wiferace", wiferace_vars)

# income
oldvars = c("v81", "v529", "v1514", "v2226", "v2852", "v3256", "v3676", "v4154", "v5029",
            "v5626", "v6173", "v6766", "v7412", "v8065", "v8689", "v9375", "v11022", "v12371",
            "v13623", "v14670", "v16144", "v17533", "v18875", "v20175", "v21481", "v23322",
            "er4153", "er6993", "er9244", "er12079", "er16462", "er20456", "er24099", "er28037",
            "er41027", "er46935", "er52343", "er58152", "er65349", "er71426")

income_vars = paste("income", years)
renameColumns(data, hash(oldvars, income_vars))

# education
oldvars = c("er30010", "er30052", "er30076", "er30100", "er30126", "er30147", "er30169", "er30197",
            "er30226", "er30255", "er30296", "er30326", "er30356", "er30384", "er30413", "er30443",
            "er30478", "er30513", "er30549", "er30584", "er30620", "er30657", "er30703", "er30748",
            "er30820", "er33115", "er33215", "er33315", "er33415", "er33516", "er33616", "er33716",
            "er33817", "er33917", "er34020", "er34119", "er34230", "er34349", "er34548")

syears = years[-which(years == 1969)]
education_vars = paste0("education", syears)
renameColumns(data, hash(oldvars, education_vars))

education_vars = paste0("education", years)
fillMissingColumns(data, "^education", education_vars)

# family size
oldvars = c("v115", "v549", "v1238", "v1941", "v2541", "v3094", "v3507", "v3920", "v4435", "v5349",
            "v5849", "v6461", "v7066", "v7657", "v8351", "v8960", "v10418", "v11605", "v13010",
            "v14113", "v15129", "v16630", "v18048", "v19348", "v20650", "v22405", "er2006",
            "er5005", "er7005", "er10008", "er13009", "er17012", "er21016", "er25016", "er36016",
            "er42016", "er47316", "er53016", "er60016", "er66016")

famsize_vars = paste0("famsize", years)
renameColumns(data, hash(oldvars, famsize_vars))

# house ownership
oldvars = c("v103", "v593", "v1264", "v1967", "v2566", "v3108", "v3522", "v3939", "v4450", "v5364",
            "v5864", "v6479", "v7084", "v7675", "v8364", "v8974", "v10437", "v11618", "v13023",
            "v14126", "v15140", "v16641", "v18072", "v19372", "v20672", "v22427",
            "er2032", "er5031", "er7031", "er10035", "er13040", "er17043", "er21042",
            "er25028", "er36028", "er42029", "er47329", "er53029", "er60030", "er66030")

houseown_vars = paste0("houseown", years)
renameColumns(data, hash(oldvars, houseown_vars))

# outcomes

# depression K6 scale, depression
# items, not sum of scale

items_depression = list(
                        item1 = c("er19828", "er23262", "er40396", "er46369",
                                  "er51730", "er57476", "er64598", "er70674"),
                        item2 = c("er19829", "er23263", "er40397", "er46370",
                                  "er51731", "er57477", "er64599", "er70675"),
                        item3 = c("er19830", "er23264", "er40398", "er46371",
                                  "er51732", "er57478", "er64600", "er70676"),
                        item4 = c("er19831", "er23265", "er40399", "er46372",
                                  "er51733", "er57479", "er64601", "er70677"),
                        item5 = c("er19832", "er23266", "er40400", "er46373",
                                  "er51734", "er57480", "er64602", "er70678"),
                        item6 = c("er19833", "er23267", "er40401", "er46374",
                                  "er51735", "er57481", "er64603", "er70679")
                        )

names_list = names(items_depression)
for (i in 1:length(items_depression)) {

    syears = years[years >= 2001 & years != 2005]
    assign(paste0("dep", names_list[i], "_vars"),
                  paste0(names_list[i], "dep", syears))
    renameColumns(data, hash(items_depression[[i]], get(paste0("dep", names_list[i], "_vars"))))

    assign(paste0("dep", names_list[i], "_vars"),
                  paste0(names_list[i], "dep", years))
    fillMissingColumns(data, paste0(names_list[i], "dep"), get(paste0("dep", names_list[i], "_vars")))

}

# depression WB

depressionwb_hash = hash(
                        "wb16b2a" = "depwb1",
                        "wb16b2b" = "depwb2",
                        "wb16b2c" = "depwb3",
                        "wb16b2d" = "depwb4",
                        "wb16b2e" = "depwb5",
                        "wb16b2f" = "depwb6"
                        )

renameColumns(data, depressionwb_hash)

# height

# head

# head feet
oldvars = c("er15553", "er19718", "er23133", "er27110", "er38321",
            "er44294", "er49633", "er55381", "er62503", "er68568")

syears = c(1999, years[years > 2000])
headheight_feet_vars = paste0("headheight_feet", syears)
renameColumns(data, hash(oldvars, headheight_feet_vars))

headheight_feet_vars = paste0("headheight_feet", years)
fillMissingColumns(data, "headheight_feet", headheight_feet_vars)

# head inches
oldvars = c("er15554", "er19719", "er23134", "er27111", "er38322",
            "er44295", "er49634", "er55382", "er62504", "er68569")

headheight_inches_vars = paste0("headheight_inches", syears)
renameColumns(data, hash(oldvars, headheight_inches_vars))

headheight_inches_vars = paste0("headheight_inches", years)
fillMissingColumns(data, "headheight_inches", headheight_inches_vars)

# head meters
oldvars = c("er49635", "er55383", "er62505", "er68570")
syears = years[years > 2010]
headheight_meter_vars = paste0("headheight_meter", syears)
renameColumns(data, hash(oldvars, headheight_meter_vars))

headheight_meter_vars = paste0("headheight_meter", years)
fillMissingColumns(data, "headheight_meter", headheight_meter_vars)

# wife

# wife feet
oldvars = c("er15661", "er19826", "er23260", "er27233", "er39418",
            "er45391", "er50751", "er56497", "er63619", "er69695")

syears = c(1999, years[years > 2000])
wifeheight_feet_vars = paste0("wifeheight_feet", syears)
renameColumns(data, hash(oldvars, wifeheight_feet_vars))

wifeheight_feet_vars = paste0("wifeheight_feet", years)
fillMissingColumns(data, "wifeheight_feet", wifeheight_feet_vars)

# wife inches
oldvars = c("er15662", "er19827", "er23261", "er27234", "er39419",
            "er45392", "er50752", "er56498", "er63620", "er69696")

wifeheight_inches_vars = paste0("wifeheight_inches", syears)
renameColumns(data, hash(oldvars, wifeheight_inches_vars))

wifeheight_inches_vars = paste0("wifeheight_inches", years)
fillMissingColumns(data, "wifeheight_inches", wifeheight_inches_vars)

# wife meters
oldvars = c("er50753", "er56499", "er63621", "er69697")
syears = years[years > 2010]
wifeheight_meter_vars = paste0("wifeheight_meter", syears)
renameColumns(data, hash(oldvars, wifeheight_meter_vars))

wifeheight_meter_vars = paste0("wifeheight_meter", years)
fillMissingColumns(data, "wifeheight_meter", wifeheight_meter_vars)

# weight

# head weight
oldvars = c("v13438", "er15552", "er19717", "er23132", "er27109", "er38320",
            "er44293", "er49631", "er55379", "er62501", "er68566")

syears = c(1986, years[years > 1998])

headweight_vars = paste0("headweight", syears)
renameColumns(data, hash(oldvars, headweight_vars))

headweight_vars = paste0("headweight", years)
fillMissingColumns(data, "headweight", headweight_vars)

# head kilograms
oldvars = c("er49632", "er55380", "er62502", "er68567")
syears = years[years > 2010]

headweight_kilos_vars = paste0("headweight_kilos", syears)
renameColumns(data, hash(oldvars, headweight_kilos_vars))

headweight_kilos_vars = paste0("headweight_kilos", years)
fillMissingColumns(data, "headweight_kilos", headweight_kilos_vars)

# wife weight
oldvars = c("v13473", "er15660", "er19825", "er23259", "er27232",
            "er39417", "er45390", "er50749", "er56495", "er63617",
            "er69693")

syears = c(1986, years[years > 1998])
wifeweight_vars = paste0("wifeweight", syears)
renameColumns(data, hash(oldvars, wifeweight_vars))

wifeweight_vars = paste0("wifeweight", years)
fillMissingColumns(data, "wifeweight", wifeweight_vars)

# wife kilograms
oldvars = c("er50750", "er56496", "er63618", "er69694")

syears = years[years > 2010]

wifeweight_kilos_vars = paste0("wifeweight_kilos", syears)
renameColumns(data, hash(oldvars, wifeweight_kilos_vars))

wifeweight_kilos_vars = paste0("wifeweight_kilos", years)
fillMissingColumns(data, "wifeweight_kilos", wifeweight_kilos_vars)

# health (good or poor)
oldvars = c("er30527", "er30598", "er30634", "er30671", "er30719", "er30764", "er30827",
            "er33117", "er33217", "er33317", "er33417", "er33517", "er33617", "er33717",
            "er33818", "er33918", "er34021", "er34120", "er34231", "er34381", "er34580")

syears = c(1986, 1988:1997, seq(1999, 2017, 2))
healthgood_vars = paste0("healthgood", syears)
renameColumns(data, hash(oldvars, healthgood_vars))

healthgood_vars = paste0("healthgood", years)
fillMissingColumns(data, "^healthgood", healthgood_vars)

# health status head
oldvars = c("v10877", "v11991", "v13417", "v14513", "v15993", "v17390", "v18721", "v20021",
            "v21321", "v23180", "er3853", "er6723", "er8969", "er11723", "er15447", "er19612",
            "er23009", "er26990", "er38202", "er44175", "er49494", "er55244", "er62366", "er68420")

syears = c(1984:1997, seq(1999, 2017, 2))
headhealth_vars = paste0("headhealth", syears)
renameColumns(data, hash(oldvars, headhealth_vars))

headhealth_vars = paste0("headhealth", years)
fillMissingColumns(data, "^headhealth", headhealth_vars)

# health status wife
oldvars = c("v10884", "v12344", "v13452", "v14524", "v15999", "v17396", "v18727", "v20027", "v21328",
            "v23187", "er3858", "er6728", "er8974", "er11727", "er15555", "er19720", "er23136",
            "er27113", "er39299", "er45272", "er50612", "er56360", "er63482", "er69547")

syears = c(1984:1997, seq(1999, 2017, 2))
wifehealth_vars = paste0("wifehealth", syears)
renameColumns(data, hash(oldvars, wifehealth_vars))

wifehealth_vars = paste0("wifehealth", years)
fillMissingColumns(data, "^wifehealth", wifehealth_vars)

# health status all
oldvars = c("er33128", "er33284", "er33326")
syears = 1994:1994
gstatus_health_vars = paste0("gstatus_health", syears)
renameColumns(data, hash(oldvars, gstatus_health_vars))
gstatus_health_vars = paste0("gstatus_health", years)
fillMissingColumns(data, "^gstatus_health", gstatus_health_vars)

# smoking

# head
# head currently smoking

oldvars = c("v13441", "er15543", "er19708", "er23123", "er27098",
            "er38309", "er44282", "er49620", "er55368", "er62490", "er68555")

syears = c(1986, years[years> 1998])
headsmoking_vars = paste0("headsmoking", syears)
renameColumns(data, hash(oldvars, headsmoking_vars))
headsmoking_vars = paste0("headsmoking", years)
fillMissingColumns(data, "^headsmoking", headsmoking_vars)

# head number of cigs by day
oldvars = c("v13442", "er15544", "er19709", "er23124", "er27099",
            "er38310", "er44283", "er49621", "er55369", "er62491", "er68556")

syears = c(1986, years[years> 1998])
headnsmoking_vars = paste0("headnsmoking", syears)
renameColumns(data, hash(oldvars, headnsmoking_vars))
headnsmoking_vars = paste0("headnsmoking", years)
fillMissingColumns(data, "^headnsmoking", headnsmoking_vars)

# head ever smoking
oldvars = c("v13444", "er15546", "er19711", "er23126", "er27101",
            "er38312", "er44285", "er49623", "er55371", "er62493", "er68558")

syears = c(1986, years[years> 1998])
headeversmoking_vars = paste0("headeversmoking", syears)
renameColumns(data, hash(oldvars, headeversmoking_vars))
headeversmoking_vars = paste0("headeversmoking", years)
fillMissingColumns(data, "^headeversmoking", headeversmoking_vars)

# wife
# wife currently smoking

oldvars = c("v13476", "er15651", "er19816", "er23250", "er27221",
            "er39406", "er45379", "er50738", "er56484", "er63606", "er69682")

syears = c(1986, years[years> 1998])
wifesmoking_vars = paste0("wifesmoking", syears)
renameColumns(data, hash(oldvars, wifesmoking_vars))
wifesmoking_vars = paste0("wifesmoking", years)
fillMissingColumns(data, "^wifesmoking", wifesmoking_vars)

# wife number of cigs by day
oldvars = c("v13477", "er15652", "er19817", "er23251", "er27222",
            "er39407", "er45380", "er50739", "er56485", "er63607", "er69683"
)

syears = c(1986, years[years> 1998])
wifensmoking_vars = paste0("wifensmoking", syears)
renameColumns(data, hash(oldvars, wifensmoking_vars))
wifensmoking_vars = paste0("wifensmoking", years)
fillMissingColumns(data, "^wifensmoking", wifeeversmoking_vars)

# wife ever smoking
oldvars = c("v13479", "er15654", "er19819", "er23253", "er27224",
            "er39409", "er45382", "er50741", "er56487", "er63609", "er69685")

syears = c(1986, years[years> 1998])
wifeeversmoking_vars = paste0("wifeeversmoking", syears)
renameColumns(data, hash(oldvars, wifeeversmoking_vars))
wifeeversmoking_vars = paste0("wifeeversmoking", years)
fillMissingColumns(data, "^wifeeversmoking", headeversmoking_vars)

# transform to long format
data = data[, lapply(.SD, as.numeric)]
list_of_vars = list(fn_vars, sn_vars, type_vars, whynoresp_vars, age_vars,
                    headrace_vars, wiferace_vars, respondent_vars, isrespondent_vars,
                    income_vars, education_vars, famsize_vars, houseown_vars, relationhead_vars,
                    healthgood_vars, headhealth_vars, wifehealth_vars, gstatus_health_vars,
                    headheight_feet_vars, headheight_inches_vars, headheight_meter_vars,
                    wifeheight_feet_vars, wifeheight_inches_vars, wifeheight_meter_vars,
                    headweight_vars, headweight_kilos_vars,
                    wifeweight_vars, wifeweight_kilos_vars,
                    depitem1_vars, depitem2_vars, depitem3_vars, depitem4_vars,
                    depitem5_vars, depitem6_vars,
                    headsmoking_vasr, headnsmoking_vars, headeversmoking_vars,
                    wifesmoking_vasr, wifensmoking_vars, wifeeversmoking_vars)
name_vars = c("fn", "sn", "type", "whynoresp", "age", "race_head", "race_wife",
              "respondent", "isrespondent", "income", "education", "famsize",
              "house_ownership", "relation_head", "health_good",
              "head_health", "wife_health", "general_health",
              "head_height_feet", "head_height_inches", "head_height_meters",
              "wife_height_feet", "wife_height_inches", "wife_height_meters",
              "head_weight", "head_weight_kilos", "wife_weight", "wife_weight_kilos",
              "depression_item1", "depression_item2", "depression_item3", "depression_item4",
              "depression_item5", "depression_item6",
              "head_smoking", "head_smoking_number", "head_smoking_ever",
              "wife_smoking", "wife_smoking_number", "wife_smoking_ever")
id_vars = c("pid", "pn", "sex", "mother_born_year", "marital_status_mother_at_birth",
            "birth_weight", "wellbeing_sample", paste0("depwb", 1:6))

ldata = melt(data, id.vars = id_vars,
             measure = list_of_vars,
             value.name = name_vars,
             variable = "wave")

years_data = data.table(wave = 1:40, year = years)
ldata = merge(ldata, years_data, by = "wave")

setorder(ldata, pid, year)

# explore
# ids = unique(ldata$pid)
# ldata[pid == sample(ids, 1)]

# identify mothers and merge datasets
gender = data[, .(pid, sex)]
mfamily = merge(family,
                gender,
                by.x = "pid_parent", by.y = "pid")
mfamily = mfamily[sex == 2]
mothers = merge(mfamily, ldata[, .(pid, year, age)], by.x = "pid_parent", by.y = "pid")
setnames(mothers,
         c("pid_kid", "age", "year"),
         c("pid", "age_mother", "first_year")
         )

# define outcome variables per individual

# individual health
# ldata[, individual_health := NA]
ldata[sn == 1 & relation_head %in% c(1, 10),
      individual_health := head_health]
ldata[sn  == 2 & relation_head %in% c(2, 20, 22),
      individual_health := wife_health]
ldata[year == 1986 & is.na(individual_health) & health_good > 0,
      individual_health := health_good]
ldata[year %in% 1994:1996 & is.na(individual_health) & general_health > 0,
      individual_heatth := general_health]
ldata[year > 1986 & is.na(individual_health) & health_good > 0,
      individual_health_binary := health_good]

# check consistency
setorder(ldata, sn, pid)
fns = unique(ldata[year == 1996, fn])
ldata[sn > 0 & year == 1996 & fn == sample(fns, 1), .(pid, fn, year, age, whynoresp, famsize,
                                             relation_head, sex, sn, head_health,
                                             wife_health, health_good, individual_health, general_health)]

# five cases with inconsistencies
ldata[year > 1986 & health_good %in% c(1,5) & individual_health %in% c(2,3,5), .(pid, year, fn)]

# coding of self-report health
# 1 Excellent
# 2 Very good
# 3 Good
# 4 Fair
# 5 Poor

# binary version
#1 Yes, is in poor health
# 5 No, is not in poor health

ldata[individual_health == 0 | individual_health > 5, individual_health := NA]
ldata[, health_binary := ifelse(individual_health %in% 1:3, 1, 0)]
ldata[is.na(individual_health),
      health_binary := ifelse(individual_health_binary == 5, 1, 0)]

table(ldata$health_binary)
fns = unique(ldata[year == 1996, fn])
ldata[sn > 0 & year == 1996 & fn == sample(fns, 1), .(pid, fn, year, age, whynoresp, relation_head,
                                             relation_head, sex, sn, head_health,
                                             wife_health, health_good, individual_health, individual_health_binary, general_health,
                                             health_binary)]

# bmi
ldata[sn == 1 & relation_head %in% c(1, 10),
      `:=` (
            height_feet = head_height_feet,
            height_inches = head_height_inches,
            height_meters = head_height_meters,
            weight_pounds = head_weight,
            weight_kilos = head_weight_kilos
            )
      ]

ldata[sn  == 2 & relation_head %in% c(2, 20, 22),
      `:=` (
            height_feet = wife_height_feet,
            height_inches = wife_height_inches,
            height_meters = wife_height_meters,
            weight_pounds = wife_weight,
            weight_kilos = wife_weight_kilos
            )
      ]

ldata[weight_pounds %in% c(0, 998, 999), weight_pounds := NA]
ldata[height_feet %in% c(0, 8, 9), height_feet := NA]
ldata[height_inches %in% c(0, 98, 99), height_inches := NA]

ldata[, bmi := 703 * weight_pounds / (height_inches + height_feet * 12) ^ 2]

# adjust metric system
ldata[!is.na(weight_kilos) & is.na(height_meters) & !is.na(height_feet) & !is.na(height_inches),
      height_meters := height_feet * .3 + height_inches * .025]

ldata[is.na(weight_kilos) & !is.na(height_meters) & is.na(weight_pounds),
      weight_kilos := weight_pounds / 2.2046]

ldata[height_meters %in% c(0, 8, 9), height_meters := NA]
ldata[weight_kilos %in% c(0, 998, 999), weight_kilos := NA]
ldata[is.na(bmi), bmi := weight_kilos / (height_meters ^ 2)]

ldata[bmi > 40, bmi := 40]
ldata[bmi < 15, bmi := 15]

setorder(ldata, year, pid)
ids = unique(ldata$pid)
ldata[pid == sample(ids, 1), .(pid, year, age,
       relation_head, bmi, weight_pounds, height_inches, height_feet)]

# depression
ldata[, isrespondent := ifelse(isrespondent == 1, 1, 0)]
ldata[is.na(isrespondent), isrespondent := 0]
table(ldata$isrespondent)
ldata[isrespondent == 1, `:=`
            (
             dep1 = depression_item1,
             dep2 = depression_item2,
             dep3 = depression_item3,
             dep4 = depression_item4,
             dep5 = depression_item5,
             dep6 = depression_item6
            )]


dep_vars = paste0("dep", 1:6)
rdep_vars = paste0("rdep", 1:6)
ldata[, (dep_vars) := lapply(.SD, function (x) ifelse(x %in% c(0, 8, 9), NA, x)), .SDcols = dep_vars]
ldata[, (rdep_vars) := lapply(.SD, reverseScale), .SDcols = dep_vars]

# depression wb
depwb_vars = paste0("depwb", 1:6)
rdepwb_vars = paste0("rdepwb", 1:6)

ldata[, (depwb_vars) := lapply(.SD, function (x) ifelse(x %in% c(9), NA, x)), .SDcols = depwb_vars]
ldata[, (rdepwb_vars) := lapply(.SD, reverseScale), .SDcols = depwb_vars]

ldata[, depression := apply(.SD, 1, mean, na.rm = TRUE), .SDcol = rdep_vars]
ldata[, depression_wb := apply(.SD, 1, mean, na.rm = TRUE), .SDcol = rdepwb_vars]
ldata[year == 2017, depression := ifelse(is.na(depression), depression_wb, depression)]
ldata[, depression := as.numeric(depression)]

setorder(ldata, year, pid)
ids = unique(ldata$pid)
ldata[pid == sample(ids, 1), .(pid, year, age,
       relation_head, depression, depression_wb)]


# ######################################
# # define cohort of interest
# # respondents born between 1976 and 1985
# ####################################

# setorder(ldata, year, pid)
# ldata[, lag_sn := shift(sn), pid]
# ldata[, lag_type := shift(type, fill = 0), pid]
# ldata[, psid_born := 0]
# ldata[sn > 0 & lag_type == 9 & (pn %in% 30:169), psid_born := 1]
# ldata[, psid_born := cumsum(psid_born), pid]
# ldata = ldata[psid_born == 1]
# ldata[, first_year := min(year), pid]
# ldata = ldata[first_year > 1975 & first_year < 1986]

# # merge with mother's age
# ldata = merge(ldata,
#              mothers[, .(pid, first_year, age_mother)],
#              by = c("first_year", "pid"), all.x = TRUE)


# names(ldata)
# ids = unique(ldata[year == 2017 & !is.na(depression), pid])
# ids = unique(ldata$pid)
# print(paste0("Total number of respondents: ", length(ids)))


# # 6300 people
# length(unique(ldata$pid))
# length(unique(ldata[wellbeing_sample == 1, pid]))

# # To create a cross-year Reference Person (‘Head’ prior to 2017)/Spouse file:
# # These concepts can be expanded to subset persons who have been the Reference Person
# # over a period of years--the yearly values for Sequence Number must be 1, and 1 or 10
# # for Relationship to Reference Person. As a corollary, to select individuals who have been
# # either Reference Persons or Spouses/Partners, yearly Sequence Numbers must equal 1 or 2
# # and yearly Relationships to Reference Person must be in the range 1, 2, 10, 20, or 22.
# # Once that subset is made and family data are merged, information about an individual can be
# # found in Reference Person variables (Reference Person's work hours,
# # Reference Person's labor income, etc.) when his or her Relationship to
#  # Reference Person=1 or 10. When Relationship to Reference Person is 2, 20, or 22,
#  # then his or her information is found in variables about the Spouse/Partner.

# saveRDS(ldata, "ch03/output/psid_eligible_respondents.rds")

