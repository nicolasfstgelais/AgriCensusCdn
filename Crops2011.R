# R code written by Stephanie shousha & Nicolas F. St-Gelais
# 2017-04-19
# Updated MM/DD/YYYY
# Code verified by : 
# -----------------------------------------------------------------------
# Crops2011.R
# .....
# 
# -----------------------------------------------------------------------

#functions
#functions1111
#Maintenant je veux ramener les colonnes de AdministrativeRegion et Code en avant. 
column.swap<-function(data,names)
{
  dataswap=cbind(data[,names,drop=F],data[,!colnames(data)%in%names])
  return(dataswap)
}

#extract the code between square brackets
addCode= function(data)
  {
code=gsub(".*\\[(.*)\\].*", "\\1",data$GEO)
data$code=code
data=column.swap(data,"code")
return(data)
}

#installing and loading the repmis package
#install.packages("repmis")
library("repmis")
#import directly from dropbox
URL <- paste0("https://dl.dropboxusercontent.com/u/11450575/HayAndCropFields%28004-0213%29.csv")

# Download data from dropbox
QC<- repmis::source_data(URL,sep = ",",header = TRUE)

#How to import manually (because R says the file is too big)
#To find the name and path of a file, go on Finder, on the document you want to import, right click -> Get Info -> Where (Path)
#QC=read.csv("/Users/Ocean/Documents/UdeM Doctorat/NANI:NAPI/2011/Crops/HayAndCropFields(004-0213).csv") 
#The path is only until the folder of the document. You need to enter its name manually. (also in Get Info -> Name and Extension)

reduce<-function(colToMerge=c("CROPS","UOM"),value="value"){
  #Je crée un truc nouveau et je l'appelle PR.QC. De QC, je veux extraire toutes les rangées qui ont "Quebec" dans la colonne GEO
  PR.QC=QC[grep("Quebec",QC$GEO),]
  #Dans PR.QC, les colonnes CROPS et UOM sont collées ensemble avec "paste" 
  #Quand on transforme le fichier de long en wide, on veut pas trop trop de colonnes. CROPS et UOM sont des variables qui vont être mises en colonnes, donc on les regroupe pour minimiser les colonnes qui vont être créées
  PR.QC$CROPSUOM=apply(PR.QC[,colToMerge],1,paste,collapse=",")
  
  #Une colonne d'intérêt a un nombre X de rangées. Par défaut, les valeurs dans ces rangées sont vues comme catégories, ou niveaux. 
  #Ce ne sont pas les valeurs de chaque rangée qui sont understood par R. Pour changer ça, il faut utiliser as.character. 
  #as.character transforme les valeurs des rangées in actual values, so R understands them as characters and not levels. 
  #Après, pcq on veut que ce soit des nombres (chiffres), on utilise as.numeric. 
  PR.QC$Value=as.numeric(as.character(PR.QC$Value)) 
  
  #Format long à wide:
  #Maintenant que j'ai sorti les rangées qui ne sont que du Québec (ce qui est intéressant pour moi), je vais switcher le format de mon fichier de long à wide. 
  #Chaque province, MRC, et municipalité aura une rangée. Je nomme ce nouveau fichier Liste. 
  # 
  drop=colnames(PR.QC)[!colnames(PR.QC)%in%c("GEO",value,"CROPSUOM")]
  Liste=reshape(PR.QC,idvar="GEO",v.names=value,timevar=c("CROPSUOM"),direction="wide",drop=drop)
  #PR.QC$Value #this is just to see what the Value column looks like in PR.QC 
  
  # create a list or mrcs with the code
  mrcs=Liste[grep("CD",Liste$GEO),]
  mrcs=addCode(mrcs)
  mrcs$codeMRC=substr(mrcs$code,5,8)
  temp=do.call(rbind,strsplit(as.character(mrcs$GEO),","))
  mrcs$mrc=temp[,1]
  mrcs=column.swap(mrcs,c("codeMRC","mrc"))
  
}






#regions=Liste[grep("CAR",Liste$GEO),]

#extract municipalities, extract code and add mrc column
municips=Liste[grep("CCS",Liste$GEO),]
municips=addCode(municips)
codeMRC=substr(municips$code,6,9)
municips$mrc=NA

#associate an mrc code to each municipality
for(i in unique(codeMRC)){
  index=which(codeMRC==i)
  municips[index,"mrc"]=mrcs[mrcs$codeMRC==i,"mrc"]
}

temp=do.call(rbind,strsplit(as.character(municips$GEO),","))
municips$municipalite=temp[,1]
municips=column.swap(municips,c("mrc","municipalite"))



#JO veut les rangées des MRCs aussi. On ne peut pas juste extraire les rangées qui ont le CCS (C'est l'abbréviation de Census Consolidation Subdivision. Ça correspond aux trois derniers chiffres du code à 9 chiffres du recensement. 
#Les 2 premiers: Province (PR), les 2 suivants: Census Agricultural Region (CAR) ou bien une des 16 régions du QC, les 2 suivants: Census Division (CD) ou bien les codes des MRCs, les 3 derniers: CCS ou bien les codes des municipalités. 
#J'ai trouvé les codes sur le siteweb de StatsCan et je les ai sauvegardés dans le dossier UdeM Doc -> NANI-NAPI. )

#Ce qui m'intéresse ce sont les régions de la Lanaudière (2407), Outaouais (2408), Laurentides (2409)
#Muni1=Liste[grep("2407",Liste$GEO),]
#Muni2=Liste[grep("2408",Liste$GEO),]
#Muni3=Liste[grep("2409",Liste$GEO),]
#Muni = rbind(Muni1,Muni2,Muni3)


#Je veux séparer les codes des noms des municipalités. 
#Je vais nommer ce fichier Test.
#Split the column GEO (from Muni) at the comma. Use as.character to make sure the GEO column is understood correctly by R. 
#And then bind them by rows (WHY DO WE DO THIS)
#Test=do.call(rbind,strsplit(as.character(Muni$GEO),","))
#For all the rows, the first column of Test is to be called AdministrativeRegion, in Muni
#Muni$AdministrativeRegion=Test[,1]
#Using gsub replaces Quebec by a space. I don't care about the name Quebec, all the AdministrativeRegions are in QC, we know this. 
#Test[,2]=gsub("Quebec ","",Test[,2])
#In Test, the second column for all the rows is called Code. 
#Muni$Code=Test[,2]
#Muni=column.swap(Muni,c("AdministrativeRegion","Code"))


#Ici, les MRC des Laurentides ont les codes Census Division 72 à 79. Leurs noms sont écrits ligne 56 (2 lignes plus bas)
#Number=c(52,60,61,62,63,64,72,73,74,75,76,77,78,79,80,81,82,83,84)
#Name=c("D'Autray","L'Assomption","Joliette","Matawinie","Montcalm","Les Moulins","Deux-Montagnes","Therese de Blainville","Mirabel","Riviere du Nord","Argenteuil","Pays d'en Haut","Laurentides","Antoine-Labelle","Papineau","Gatineau","Les Collines-de-l'Outaouais","La Vallée-de-la-Gatineau","Pontiac")
#Je crée un dossier qui s'appelle CensusDiv: code du CD de la MRC vs son nom
#CensusDiv=data.frame(Number,Name)

#Ici c'est un loop que pour chaque rangée dans CensusDiv, 
#for(i in 1:nrow(CensusDiv))
 # {
  #je vais coller ensemble le code CCS2409 et le CensusDiv Number (de 72 à 79) with NO separation. 
  #CodeTemp=paste("CCS2409",CensusDiv$Number[i],sep="")
  
  #J'extrais quelque chose, ajoute colonne MRC
  #Try=grep(CodeTemp,MuniLau[,"Code"])
  #MuniLau[Try,"MRC"]=as.character(CensusDiv$Name[i])
#}


#Test4=column.swap(MuniLau,c("MRC","AdministrativeRegion","Code"))
#Muni=column.swap(Muni,c("MRC","AdministrativeRegion","Code"))

#Je veux exporter MuniLau en excel pour l'envoyer à JO
#library(xlsx)
#write.xlsx(MuniLau,"/Users/Ocean/Documents/UdeM Doctorat/NANI:NAPI/2011/Crops/MuniLau.xlsx")
write.csv(municips,"muni.csv",)
