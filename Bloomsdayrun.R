#Bloomsday Run

library(XML)
library(reshape2)
library(ggplot2)

#2013 Geography Stats - Extraction
URL1 <- "http://bloomsdayrun.org/history-stats/geographic-stats"
tablesfromURL1 <- readHTMLTable(URL1)
geographytable<-tablesfromURL1[[1]]

# cleaning and organizing
geographytable[1,]
geographytable<-geographytable[c(1,2,5,8)]
names(geographytable) <- c("Region", "Registered", "Finished", "Did not Finish")
geographytable<-geographytable[2:8,]

levels(geographytable$Region)[levels(geographytable$Region)=="Local Finishers (Spokane)"] <- "Spokane"
geographytable$Region<-factor(geographytable$Region)
geographytablemelt<-melt(geographytable,id.vars="Region")
geographytablemelt$value<-gsub(",", "", geographytablemelt$value)
geographytablemelt$value<-as.numeric(geographytablemelt$value)

# Aligning factor levels to Region of participation so in order to make the legend easier to understand.: 
#Thanks: http://learnr.wordpress.com/2010/03/23/ggplot2-changing-the-default-order-of-legend-labels-and-stacking-of-data/
geographytablemelt$Region<-factor(geographytablemelt$Region,levels = c("Spokane","Other Washington","Idaho","Montana","Oregon","Canada","Other Locations"))

# The plot
ggplot(geographytablemelt,aes(x=variable,y=value,fill=Region))+geom_bar(postion="dodge",stat="identity")+theme(axis.text.x = element_text(color="black"))+ 
  theme(axis.text.y = element_text(color="black"))+ 
  ggtitle("Bloomsday 2013 Geographic Participation") + 
  labs(y="Number of People",x="")

#Gender Information
URL2 <- "http://bloomsdayrun.org/history-stats/gender-stats"
tablesfromURL2 <- readHTMLTable(URL2)
gender<-tablesfromURL2[[1]]

# cleaning, organizing
gender[1,]
names(gender) <- c("Year", "Male", "Female")
gender<-gender[-1,]
gender$Male<-gsub("%", "", gender$Male)
gender$Male<-as.numeric(as.character(gender$Male))
gender$Female<-gsub("%","",gender$Female)
gender$Female<-as.numeric(as.character(gender$Female))
gender$Year<-as.numeric(levels(gender$Year))[gender$Year]

#Proportional Stacked Area Graph for Gender
gendermelted<-melt(gender,id=c("Year"),measured=c("Male","Female"))

# The plot

ggplot(gendermelted, aes(x=Year, y=value, fill=variable)) + 
  geom_area(alpha=.7) +
  labs(x = "Year", y = "Percent of Finishers")+scale_x_continuous(breaks=gendermelted$Year) +
  theme(axis.text.x = element_text(color="black",angle=-90, hjust=1, vjust=1),axis.text.y = element_text(color="black")) + 
  annotate("text", x=2006, y=25, label="Male",size=5) + 
  annotate("text", x=2006, y=75, label="Female",size=5)+
  guides(fill=FALSE)+
  annotate("text", x=1979, y=73, label="72.7%",size=5)+ 
  annotate("text", x=2012, y=43, label="40.3%",size=5)+
  ggtitle("Percent of Male and Female Finishers over Years")

#Race Director/President Information
URL3 <- "http://bloomsdayrun.org/history-stats/presidentsrace-directors"
tablesfromURL3 <- readHTMLTable(URL3)
racepresident<-tablesfromURL3[[2]]
# cleaning
racepresident[1,]
names(racepresident) <- c("Year","President")
racepresident<-racepresident[-1,]
racepresident$Year<-as.numeric(levels(racepresident$Year))[racepresident$Year]

#Race Directors, in alpha ordering

ggplot(racepresident,aes(x=Year,y=President,color=President))+ 
  geom_point(size=5)+ 
  scale_x_continuous(breaks=racepresident$Year)+ 
  theme(axis.text.x = element_text(color="black",angle=-90, vjust=.5))+ 
  theme(axis.text.y = element_text(color="black"))+ 
  ggtitle("Presidents/Race Directors over Years (in Alphabetical order by First name)") + 
  labs(y="President/Director") + theme(legend.position="none")

#Race Directors, ordered on # of times served: Reorder function from Richie Cotton:http://stackoverflow.com/questions/5804226/get-a-histogram-plot-of-factor-frequencies-summary
fac_levels <- levels(racepresident$President); 
o <- order(table(racepresident$President)); 
racepresident$Presidentord <- with(racepresident, 
                                       factor(President, levels = fac_levels[o]))

ggplot(racepresident,aes(x=Year,y=Presidentord,color=Presidentord))+ 
  geom_point(size=5)+ scale_x_continuous(breaks=racepresident$Year)+
  theme(axis.text.x = element_text(color="black",angle=-90, vjust=.5))+ 
  theme(axis.text.y = element_text(color="black"))+ 
  ggtitle("Presidents/Race Directors over Years (in order of Number of Times an 
          Individual was President/Director)") + 
  labs(y="President/Director") + theme(legend.position="none")


#Race Winners in different categories
URL4 <- "http://bloomsdayrun.org/results/results-history/general-(1977plus)"
tablesfromURL4 <- readHTMLTable(URL4)
genresults<-tablesfromURL4[[1]]
wheelchairresults<-tablesfromURL4[[2]]

# cleaning
genresults[1,]
names(genresults) <- c("Year", "General.Registered", "General.Finished", "General.Men.Winner", "General.Women.Winner")
genresults<-genresults[-1,]
genresults$Year<-as.numeric(levels(genresults$Year))[genresults$Year]
genregfin<-genresults[c(1:3)]
genresults<-genresults[c(1,4:5)]

#graph of number of finishers versus registered in the general category

genregfinmelt<-melt(genregfin,id=c("Year"),measured=c("General.Registered","General.Finished"))

# There are commas in the value column to denote thousands... so let's remove them and convert that var to numeric
genregfinmelt$value<-gsub(",", "", genregfinmelt$value)
genregfinmelt$value<-as.numeric(genregfinmelt$value)
names(genregfinmelt) <- c("Year", "Category", "Value")
ggplot(genregfinmelt,aes(x=Year,y=Value,color=Category))+geom_line(size=1)+scale_x_continuous(breaks=genregfinmelt$Year)+
  theme(axis.text.x = element_text(color="black",angle=-90, vjust=.5))+ 
  theme(axis.text.y = element_text(color="black"))+ 
  ggtitle("Number of People Registered and Finished in the General Category") + 
  labs(y="",x="")
  

genresultsmelt<-melt(genresults,id.vars="Year")
names(genresultsmelt) <- c("Year", "Category", "Value")
ggplot(genresultsmelt,aes(x=Year,y=reorder(Value,Value,function(x)-length(x)),color=Category))+geom_point(size=5)+
  scale_x_continuous(breaks=genresultsmelt$Year)+
  theme(axis.text.x = element_text(color="black",angle=-90, vjust=.5))+ 
  theme(axis.text.y = element_text(color="black"))+ 
  ggtitle("General Category Winners") + 
  labs(y="",x="")


wheelchairresults[1,]
names(wheelchairresults) <- c("Year", "Wheelchair.Men.Open", "Wheelchair.Women.Open", "Wheelchair.Men.Masters", "Wheelchair.Quad", "Wheelchair.T1.Quad", "Wheelchair.T2.Quad")
wheelchairresults<-wheelchairresults[-1,]

wheelchairresults$Year<-as.numeric(levels(wheelchairresults$Year))[wheelchairresults$Year]
wheelchairmelt<-melt(wheelchairresults,id.vars="Year")
# There are hyphens "-" in the value column so let's remove them and convert that var to numeric
wheelchairmelt$value<-gsub("-", "", wheelchairmelt$value)
# Above changes Santiago Sanz-Quinto's and Cornelio Nunez-Ordaz's value
wheelchairmelt$value<-gsub("Santiago SanzQuinto", "Santiago Sanz-Quinto", wheelchairmelt$value)
wheelchairmelt$value<-gsub("Cornelio NunezOrdaz", "Cornelio Nunez-Ordaz", wheelchairmelt$value)
#Insert a blank in rows 74,138 & 170 for value.... currently, some stuff we don't need
wheelchairmelt$value[74]<-""
wheelchairmelt$value[138]<-""
wheelchairmelt$value[170]<-""
wheelchairmelt$value<-as.factor(wheelchairmelt$value)
wheelchairmeltsubset<-subset(wheelchairmelt,value!="")
names(wheelchairmeltsubset) <- c("Year", "Category", "Value")

ggplot(wheelchairmeltsubset,aes(x=Year,y=Value,color=Category))+geom_point(size=5)+
scale_x_continuous(breaks=wheelchairmeltsubset$Year)+
  theme(axis.text.x = element_text(color="black",angle=-90, vjust=.5))+ 
  theme(axis.text.y = element_text(color="black"))+ 
  ggtitle("Wheelchair Winners") + 
  labs(y="",x="")


overallresults<-rbind(wheelchairmeltsubset,genresultsmelt)

#Final Overall Winners

ggplot(overallresults,aes(x=Category,y=Year))+geom_blank()+geom_rect(aes(xmin = as.numeric(Category)-.5, 
  xmax = as.numeric(Category)+.5, 
  ymin = -Inf, ymax = Inf, fill = Category),alpha=0.4)+
  geom_text(data=overallresults,aes(x=Category,y=Year,label=Value),size=4,fontface="bold")+
  scale_y_continuous(breaks=overallresults$Year)+ 
  theme(axis.text.x = element_text(color="black",vjust=.5,size=12))+ 
  theme(axis.text.y = element_text(color="black",size=10))+ 
  ggtitle("Winners in All Categories across All Years") + 
  labs(y="",x="") + theme(legend.position="none")+
  theme(panel.grid.major = element_line(colour = 'black'))

#Perennials - Extracting data directly was problematic.... so copied table to text file....
#URL5 <- "http://bloomsdayrun.org/results/perennials#"
#tablesfromURL5 <- readHTMLTable(URL5)
perennial<-read.table("Perennials.txt",sep="\t")
names(perennial) <- c("Position", "Runner", "Time")
perennial<-perennial[-1,]
perennial$Time <- as.POSIXct(perennial$Time, format = "%H:%M:%S") 
#Thanks: http://learnr.wordpress.com/2010/02/25/ggplot2-plotting-dates-hours-and-minutes/

perennial$Position<-as.numeric(as.character(perennial$Position))
ggplot(perennial,aes(x=Time,y=reorder(Runner,Position)))+geom_point(aes(color="red",size=3))+
  theme(axis.text.x = element_text(color="black",vjust=.5))+ 
  theme(axis.text.y = element_text(color="black"))+ 
  ggtitle("The Perennials: Finish Times (in hours) of Runners Who Participated in and Completed ALL Bloomsday Runs") + 
  labs(y="",x="") + theme(legend.position="none")

# Age Group Record Winners
URL5 <- "http://bloomsdayrun.org/results/results-history/age-group-records"
tablesfromURL5 <- readHTMLTable(URL5)
Maleagerecords<-tablesfromURL5[[1]]
Femaleagerecords<-tablesfromURL5[[2]]

# cleaning
Maleagerecords[1,]
names(Maleagerecords) <- c("Age", "Name", "Year", "Time","Location")
Maleagerecords<-Maleagerecords[-1,]
Maleagerecords$Gender<-"Male"

# cleaning
Femaleagerecords[1,]
names(Femaleagerecords) <- c("Age", "Name", "Year", "Time","Location")
Femaleagerecords<-Femaleagerecords[-1,]
Femaleagerecords$Gender<-"Female"

# Combining both together
agerecords<-rbind(Maleagerecords,Femaleagerecords)

#Cleaning: Time has an asterisk and two junk values by inserting NAs

agerecords$Time<-as.character(agerecords$Time)
agerecords$Time[2]<-"34:10"
agerecords$Location<-as.character(agerecords$Location)
agerecords$Location[14]<-""
agerecords$Location[28]<-""
agerecords$Gender<-as.factor(agerecords$Gender)
agerecords$Year<-as.numeric(as.character(agerecords$Year))
agerecords$Name<-as.character(agerecords$Name)
agerecords$Name<-paste(agerecords$Name,agerecords$Location,sep=" from ")
agerecords$Name[14]<-gsub("from", "", agerecords$Name[14])
agerecords$Name[28]<-gsub("from", "", agerecords$Name[28])
agerecords$Name<-paste(agerecords$Name,agerecords$Year,sep=",in ")

#Dealing with time var

for (i in 1:27){agerecords$Time[i] <- paste("00:",agerecords$Time[i],sep="")}
agerecords$Time[28]<-paste("0",agerecords$Time[28],sep="")

agerecords$Time <- as.POSIXct(agerecords$Time, format = "%H:%M:%S") #Thanks: http://learnr.wordpress.com/2010/02/25/ggplot2-plotting-dates-hours-and-minutes/
#-----------------------------------
ggplot(agerecords,aes(x=Time,y=Name,color=Gender))+geom_point(size=5)+facet_grid(Age~.,scales="free")+
  theme(axis.text.x = element_text(color="black"))+ 
  theme(axis.text.y = element_text(color="black"))+ 
  ggtitle("Top Performances in Each Age Group, Since 1983") + 
  labs(y="",x="Time")
