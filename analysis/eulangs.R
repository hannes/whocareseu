# Geonames localized country name retrieval for Europe, <hannes@muehleisen.org>, 2014-01-13
gncountryInfoCSV <- "http://api.geonames.org/countryInfoCSV?username=hfmuehleisen&"

# get country data from geonames API
countries <- read.csv(url(paste0(gncountryInfoCSV,"lang=en")),sep="\t",stringsAsFactors=F)
# ISO country codes for the countries in Europe
eucountrycodes <- scan(text="BE BG CZ DK DE EE IE GR ES FR HR IT CY LV LT LU HU MT NL AT PL PT RO SI SK FI SE GB NO CH",sep=" ", what=character())

#only countries in europe are interesting
eucountries <- countries[countries$iso.alpha2 %in% eucountrycodes,]

cleanuplangs <- function(langstrs) {
  langcodes <- sort(unique(sapply(strsplit(unlist(strsplit(langstrs,",")),"-"),"[[",1)))
  langcodes <- langcodes[nchar(langcodes) == 2]
  langcodes
}
langcodes <- cleanuplangs(eucountries$languages)

cat(paste0("'",langcodes,"'",collapse=", "))

# this is dirty, I know, but this gets the alternate names for holland and england, which are non-official, but used a lot
#select langcode,'GB' as country,altname from alternatenames where altid=6269131 and langcode in ('bg', 'br', 'ca', 'co', 'cs', 'cy', 'da', 'de', 'el', 'en', 'es', 'et', 'eu', 'fi', 'fo', 'fr', 'fy', 'ga', 'gd', 'gl', 'hr', 'hu', 'it', 'lb', 'lt', 'lv', 'mt', 'nb', 'nl', 'nn', 'no', 'oc', 'pl', 'pt', 'rm', 'ro', 'ru', 'sc', 'se', 'sh', 'sk', 'sl', 'sr', 'sv', 'tr') union all select langcode,'NL' as country,altname from alternatenames where altid=4996248 and langcode in ('bg', 'br', 'ca', 'co', 'cs', 'cy', 'da', 'de', 'el', 'en', 'es', 'et', 'eu', 'fi', 'fo', 'fr', 'fy', 'ga', 'gd', 'gl', 'hr', 'hu', 'it', 'lb', 'lt', 'lv', 'mt', 'nb', 'nl', 'nn', 'no', 'oc', 'pl', 'pt', 'rm', 'ro', 'ru', 'sc', 'se', 'sh', 'sk', 'sl', 'sr', 'sv', 'tr');
altnamestr <- "lang,iso.alpha2,name
es,GB,Inglaterra
it,GB,Inghilterra
eu,GB,Ingalaterra
en,GB,England
fr,GB,Angleterre
ca,GB,Anglaterra
sv,GB,England
nl,GB,Engeland
br,GB,Bro-Saoz
pt,GB,Inglaterra
hr,GB,Engleska
sk,GB,Anglicko
ga,GB,Sasana
lt,GB,Anglija
oc,GB,Anglatèrra
rm,GB,Engalterra
mt,GB,Ingilterra
gl,GB,Inglaterra
el,GB,Αγγλία
tr,GB,İngiltere
gd,GB,Sasainn
sr,GB,Енглеска
fo,GB,Ongland
sc,GB,Inghilterra
fy,GB,Ingelân
sl,GB,Anglija
se,GB,Englánda
fi,GB,Englanti
et,GB,Inglismaa
lv,GB,Anglija
cs,GB,Anglie
cy,GB,Lloegr
bg,GB,Англия
ru,GB,Англия
en,NL,Holland
fr,NL,Holland
de,NL,Holland
nl,NL,Holland
no,NL,Holland
pt,NL,Holland
ru,NL,Голландия
sr,NL,Холанд
de,GB,England"

altnames <- read.csv(text=altnamestr,sep=",")

# get the names of the countries in all the languages
countrynames <- do.call("rbind",lapply(langcodes,function(lang) {
  langcountry <- read.csv(url(paste0(gncountryInfoCSV,"lang=",lang)),sep="\t",stringsAsFactors=F)
  langcountry$lang <- lang
  langcountry[langcountry$iso.alpha2 %in% eucountrycodes,c("lang","iso.alpha2","name")]
}))

countrynames <- rbind(countrynames,altnames)

# remove bracketed things
countrynames$name <- gsub(" \\(.+\\)","",countrynames$name)

# time for some reshaping
countriesandlangs <- do.call("rbind",apply(eucountries[,c("iso.alpha2","languages")],1,function(row) {
  ret <- data.frame(lang=c("en",cleanuplangs(row[[2]])),stringsAsFactors=F)
  ret$iso.alpha2 <- row[[1]]
  ret[,c(2,1)]
}))

countrylangcountrycombos <- do.call("rbind",apply(countriesandlangs,1,function(row) {
  ret <- data.frame(othercountry=eucountrycodes,stringsAsFactors=F)
  ret$country <- row[[1]]
  ret$lang <- row[[2]]
  ret[,c(2,3,1)]
}))

row.names(countrylangcountrycombos) <- NULL
finallist <- merge(countrylangcountrycombos,countrynames,by.x=c("lang","othercountry"),by.y=c("lang","iso.alpha2"))[,c(1,3,2,4)]

# go to adwords keyword planner, "get search volumne for a list of keywords", set country, keyword ideas, download, excel format
# generate the input for the adwords thing
invisible(lapply(eucountrycodes,function(country) {
  cname <- eucountries[eucountries$iso.alpha2 == country,]$name
  cat(paste0(cname," (",country,"): ", paste0(sort(unique(finallist[finallist$country==country,]$name)),collapse=", ")),"\n\n")
  NULL
}))

stop("Now go ask Google (https://adwords.google.com/ko/KeywordPlanner/)")

# assume we have everything, load the CSVs
googleadwordsfreqs <- do.call("rbind",lapply(eucountrycodes,function(country) {
  gdata <- read.csv(paste0("eulangs-results/",country,".csv"),sep="\t",stringsAsFactors=F,fileEncoding="UTF-16")[,c(2,4)]
  gdata$country <- country
  gdata
}))

# map keywords back to countries
cckeywordcount <- merge(googleadwordsfreqs,finallist,by.x=c("country","Keyword"),by.y=c("country","name"),all.x=T)
row.names(cckeywordcount) <- NULL
# remove duplicate entries for same other country with same keyword ("Malta")
cckeywordcount <- cckeywordcount[!duplicated(cckeywordcount[,c("country","Keyword","othercountry")]),]

# aggregate avg. monthly searches by othercountry by summation
final <-aggregate(cckeywordcount$Avg..monthly.searches, by=list(cckeywordcount$country,cckeywordcount$othercountry),FUN=sum,na.rm=T)
names(final) <- c("sourcecountry","targetcountry","monavgsearches")
final <- final[order(final$sourcecountry),]
row.names(final) <- NULL

final$sourcecountry <- factor(final$sourcecountry)
final$targetcountry <- factor(final$targetcountry)
final <- final[final$targetcountry != final$sourcecountry,]


final <- merge(final,aggregate(final$monavgsearches, by=list(final$sourcecountry),FUN=sum),by.x=c("sourcecountry"),by.y=c("Group.1"))
final$searchratio <- round((final$monavgsearches/final$x)*100)

final$x <- final$monavgsearches <- NULL
write.csv(final,"results.csv",row.names=F)

bytarget <- lapply(split(final,final$targetcountry),function(group) {
  lapply(split(group,group$sourcecountry),function(group) {
    group$searchratio
  })
})

library(rjson)
bytargetjson <- paste0("var bytarget = ",toJSON(bytarget))
write(bytargetjson,"bytarget.js")

# that's all