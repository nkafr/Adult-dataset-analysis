preproccesing  <- function(adult) {

  
######### A). Data cleaning
  
#Remove duplicate obeservations   
adult=subset(adult,!duplicated(adult))
  
#Remove redundant features
adult[["education_num"]]=NULL
adult[["fnlwgt"]]=NULL

#Remove Missing Data
is.na(adult) = adult=='?'
is.na(adult) = adult==' ?'
adult=na.omit(adult)

#remove independent variable column in order to add it later
depen=adult[["over50k"]]
adult[["over50k"]]=NULL

######### B). Data Transformations

#convery sex to numerical
adult$sex=ifelse(adult$sex==adult$sex[1],1,0)

#one-hot encode type_employer
adult$Federal.gov=ifelse(grepl("Federal-gov",adult$type_employer,fixed=TRUE), 1, 0)
adult$Local.gov=ifelse(grepl("Local-gov",adult$type_employer,fixed=TRUE), 1, 0)
adult$Private=ifelse(grepl("Private",adult$type_employer,fixed=TRUE), 1, 0)
adult$Self.emp.inc=ifelse(grepl("Self-emp-inc",adult$type_employer,fixed=TRUE), 1, 0)
adult$Self.emp.not.inc=ifelse(grepl("Self-emp-not-inc",adult$type_employer,fixed=TRUE), 1, 0)
adult$State.gov=ifelse(grepl("State-gov",adult$type_employer,fixed=TRUE), 1, 0)
adult$Without.pay=ifelse(grepl("Without-pay",adult$type_employer,fixed=TRUE), 1, 0)
adult$type_employer=NULL


#one-hot encode occupation 
adult$Adm.clerical=ifelse(grepl("Adm-clerical",adult$occupation,fixed=TRUE), 1, 0)
adult$Armed.Forces=ifelse(grepl("Armed-Forces",adult$occupation,fixed=TRUE), 1, 0)
adult$Craft.repair=ifelse(grepl("Craft-repair",adult$occupation,fixed=TRUE), 1, 0)
adult$Exe.managerial=ifelse(grepl("Exec-managerial",adult$occupation,fixed=TRUE), 1, 0)
adult$Farming.fishing=ifelse(grepl("Farming-fishing",adult$occupation,fixed=TRUE), 1, 0)
adult$Handlers.cleaners=ifelse(grepl("Handlers-cleaners",adult$occupation,fixed=TRUE), 1, 0)
adult$Machine.op.inspct=ifelse(grepl("Machine-op-inspct",adult$occupation,fixed=TRUE), 1, 0)
adult$ther.service=ifelse(grepl("Other-service",adult$occupation,fixed=TRUE), 1, 0)
adult$Priv.house.serv=ifelse(grepl("Priv-house-serv",adult$occupation,fixed=TRUE), 1, 0)
adult$Prof.specialty=ifelse(grepl("Prof-specialty",adult$occupation,fixed=TRUE), 1, 0)
adult$Protective.serv=ifelse(grepl("Protective-serv",adult$occupation,fixed=TRUE), 1, 0)
adult$Sales=ifelse(grepl("Sales",adult$occupation,fixed=TRUE), 1, 0)
adult$Tech.support=ifelse(grepl("Tech-support",adult$occupation,fixed=TRUE), 1, 0)
adult$Transport.moving=ifelse(grepl("Transport-moving",adult$occupation,fixed=TRUE), 1, 0)
adult$occupation=NULL

#one-hot encode education 
adult$school=ifelse(grepl("10th",adult$education,fixed=TRUE) | grepl("11th",adult$education,fixed=TRUE) | grepl("12th",adult$education,fixed=TRUE) 
                    |grepl("1st-4th",adult$education,fixed=TRUE) |grepl("5th-6th",adult$education,fixed=TRUE) | grepl("7th-8th",adult$education,fixed=TRUE) 
                    | grepl("9th",adult$education,fixed=TRUE),1,0)
adult$pre=ifelse(grepl("Preschool",adult$education,fixed=TRUE), 1, 0)
adult$Assoc.acdm=ifelse(grepl("Assoc-acdm",adult$education,fixed=TRUE), 1, 0)
adult$Assoc.voc=ifelse(grepl("Assoc-voc",adult$education,fixed=TRUE), 1, 0)
adult$Bachelors=ifelse(grepl("Bachelors",adult$education,fixed=TRUE), 1, 0)
adult$Doctorate=ifelse(grepl("Doctorate",adult$education,fixed=TRUE), 1, 0)
adult$HS.grad=ifelse(grepl("HS-grad",adult$education,fixed=TRUE), 1, 0)
adult$Masters=ifelse(grepl("Masters",adult$education,fixed=TRUE), 1, 0)
adult$Prof.school=ifelse(grepl("Prof-school",adult$education,fixed=TRUE), 1, 0)
adult$Some.college=ifelse(grepl("Some-college",adult$education,fixed=TRUE), 1, 0)
adult$education=NULL



#one-hot encode marital 
adult$not.married=ifelse(grepl("Divorced",adult$marital,fixed=TRUE) | grepl("Married-spouse-absent",adult$marital,fixed=TRUE) 
                                                ,1, 0)
adult.separated=ifelse(grepl("Separated",adult$marital,fixed=TRUE),1,0)
adult$married=ifelse(grepl("Married-AF-spouse",adult$marital,fixed=TRUE) | grepl("Married-civ-spouse",adult$marital,fixed=TRUE) ,1, 0)
adult$never.married=ifelse(grepl("Never-married",adult$marital,fixed=TRUE),1,0)
adult$never.widowed=ifelse(grepl("Widowed",adult$marital,fixed=TRUE),1,0)
adult$marital=NULL


#one-hot encode relationship
adult$Couple=ifelse(grepl("Husband",adult$relationship,fixed=TRUE), 1, 0)
adult$Not.in.family=ifelse(grepl("Not-in-family",adult$relationship,fixed=TRUE), 1, 0)
adult$Other.relative=ifelse(grepl("Other-relative",adult$relationship,fixed=TRUE), 1, 0)
adult$Own.child=ifelse(grepl("Own-child",adult$relationship,fixed=TRUE), 1, 0)
adult$Unmarried=ifelse(grepl("Unmarried",adult$relationship,fixed=TRUE), 1, 0)
adult$Wife=ifelse(grepl("Wife",adult$relationship,fixed=TRUE), 1, 0)
adult$relationship=NULL

#convert race to numerical
adult$race = gsub("Amer-Indian-Eskimo","1",adult$race)
adult$race = gsub("Asian-Pac-Islander","2",adult$race)
adult$race = gsub("Black","3",adult$race)
adult$race = gsub("Other","4",adult$race)
adult$race = gsub("White","5",adult$race)
adult$race =as.numeric(adult$race)

#convert country to numerical
adult$country = gsub("United-States","1",adult$country)   #United States

adult$country = gsub("Ireland","2",adult$country)         #Anglosaxon Countries
adult$country = gsub("Canada","2",adult$country)
adult$country = gsub("England","2",adult$country)
adult$country = gsub("Scotland","2",adult$country)

adult$country = gsub("Italy","3",adult$country)         #Wealthy Europen Countries
adult$country = gsub("France","3",adult$country) 
adult$country = gsub("Germany","3",adult$country)
adult$country = gsub("Holand-Netherlands","3",adult$country)

adult$country = gsub("Greece","4",adult$country)         #Poor/Moderate Europen Countries
adult$country = gsub("Hungary","4",adult$country)       
adult$country = gsub("Poland","4",adult$country)
adult$country = gsub("Portugal","4",adult$country)
adult$country = gsub("South","4",adult$country)  # South Afica??
adult$country = gsub("Yugoslavia","4",adult$country)

adult$country = gsub("Guatemala","5",adult$country)       #Latin America Countries
adult$country = gsub("Dominican-Republic","5",adult$country)
adult$country = gsub("Haiti","5",adult$country)
adult$country = gsub("Honduras","5",adult$country)
adult$country = gsub("Jamaica","5",adult$country)
adult$country = gsub("Mexico","5",adult$country)
adult$country = gsub("Nicaragua","5",adult$country)
adult$country = gsub("Outlying-US\\(Guam-USVI-etc\\)","5",adult$country)
#adult$country[adult$country=="Outlying-US(Guam-USVI-etc)"] = "5"
adult$country = gsub("Puerto-Rico","5",adult$country)
adult$country = gsub("Trinadad&Tobago","5",adult$country)

adult$country = gsub("El-Salvador","6",adult$country)     #South America Countries
adult$country = gsub("Columbia","6",adult$country)
adult$country = gsub("Ecuador","6",adult$country)
adult$country = gsub("Peru","6",adult$country) 

adult$country = gsub("Cambodia","7",adult$country)       #Asian Countries
adult$country = gsub("India","7",adult$country)
adult$country = gsub("Iran","7",adult$country)
adult$country = gsub("Laos","7",adult$country)
adult$country = gsub("Philippines","7",adult$country)
adult$country = gsub("Thailand","7",adult$country)
adult$country = gsub("Vietnam","7",adult$country)

adult$country = gsub("Cuba","8",adult$country)       #Others Countries
adult$country = gsub("Japan","8",adult$country) 

adult$country = gsub("China","9",adult$country)       #China
adult$country = gsub("Hong","9",adult$country)
adult$country = gsub("Taiwan","9",adult$country)
adult$country =as.numeric(adult$country)


#normalize all variables
#scale function in R substracts the mean and divides by standard deviation for each value
adult=data.frame(lapply(adult, function(x) scale(x) ))#

#plug the dependent varible at the end
adult$over50k=depen
#convert categorical variable to numerical (>=50=1, <50=0 )
adult$over50k=ifelse(adult$over50k==adult$over50k[1],0,1)

return(adult)
}




