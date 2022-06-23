library(readr)
library(dplyr)
library(gsheet)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(ggplot2)
library(tidyverse)
library(ggthemes)
library(plotly)
library(ggrepel)
library(gridExtra)

small_cave <- read_csv("~/Desktop/Conservation_Status/Cave Species Only - Sheet1.csv")

All_data <- read_csv("~/Desktop/Conservation_Status/Nature Serve All Species  - Sheet1.csv")

Caves <- read_csv("~/Desktop/Conservation_Status/tax_and_regions.csv")
names(Caves)[8] <- "G_status"
names(Caves)[10] <- "R_status"
Caves$G_status <- factor(Caves$G_status, 
                         levels = c("G1: Critically Imperiled",
                                    "G2: Imperiled",
                                    "G3: Vulnerable",
                                    "G4: Apparently Secure",
                                    "G5: Secure",
                                    "GH: Possibly Extinct",
                                    "GX: Presumed Extinct",
                                    "GNR: Unranked" ,
                                    "GU: Unrankable")) # This is making sure that no variables get left out

#Caves <- Caves %>% 
 # mutate(G_status = ifelse(G_status == 'T1: Critically Imperiled', 'G1: Critically Imperiled', G_status))

names(Caves)


#Number of critically imperiled species by region
crit_imperiled <- Caves %>% filter(G_status %in% "G1: Critically Imperiled") %>% 
  group_by(Region) %>% 
  tally %>% 
  arrange(desc(n))


#Number of secure species by region 
secured <- Caves %>% filter (G_status %in% "G5: Secure") %>% 
  group_by(Region) %>% 
  tally %>% 
  arrange(desc(n))


#Top 10 Regions/States with the most critically imperiled species (Possibly most in need?)
crit_imperiled_top_10 <- Caves %>% filter(G_status %in% "G1: Critically Imperiled") %>% 
  group_by(Region) %>% 
  tally %>% 
  arrange(desc(n)) %>% 
  head(10)


#Lets see how all of those look when using Regional Status instead of global 
crit_imperiled_regional <- Caves %>% filter(R_status %in% "S1: Critically Imperiled") %>% 
  group_by(Region) %>% 
  tally %>% 
  arrange(desc(n))
secured_regional <- Caves %>% filter (R_status %in% "S5: Secure") %>% 
  group_by(Region, `Common Name`) %>% 
  tally %>% 
  arrange(desc(n))
# Wow so the above code is showing that there are only 7 total classified as 
#secure within the regional status (7 but really only 5 when grouping by common name )



#Let's look at what family has the most critically imperiled accross global status
crit_family <- Caves %>% filter(G_status %in% "G1: Critically Imperiled") %>% 
  group_by(Family, Region) %>% 
  tally %>% 
  arrange(desc(n))


#Now let's see the least endangered families grouped by G5: Secure
secure_family <- Caves %>% filter(G_status %in% "G5: Secure") %>% 
  group_by(Family, Region) %>% 
  tally %>% 
  arrange(desc(n))


#Let's try to make a bar chart of the different global status and the number in each category 
all_status <- Caves %>% group_by(G_status) %>% 
  distinct(Species) %>% 
  tally 
(Caves%>% group_by(G_status) %>% 
    distinct(Species)
  %>% tally)
#Caves <- Caves %>% 
 # mutate(G_status = ifelse(G_status == 'T1: Critically Imperiled', 'G1: Critically Imperiled', G_status))
bar1 <- ggplot(data = all_status, aes(x = G_status, y = n, fill = G_status)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.25, colour = "black", size = 2.75) +
  scale_fill_manual(values = c( "G1: Critically Imperiled" ='darkred',
                                "G2: Imperiled" = 'darkorange3',
                                "G3: Vulnerable" = 'darkgoldenrod2',
                                "G4: Apparently Secure" = 'chartreuse4',
                                "G5: Secure" = 'darkgreen',
                                "GH: Possibly Extinct" = 'black',
                                "GX: Presumed Extinct" = 'darkslategray4',
                                "GNR: Unranked" = 'darkslategray',
                                "GU: Unrankable" = 'darkgrey'
  )) +
  labs(x = 'Global Status', y = 'Number of Species', title = "Species Global Status") +
  theme_clean() +
  theme(legend.position = 'none') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggplotly(bar1)
plot(bar1)
#Obviously on this plot Critically imperiled species are the most abundent so let's do a bar plot 
#of the top 10 regions containing these critically imperiled speices
bar <- ggplot(data = crit_imperiled_top_10, aes(x = Region, y = n)) +
  geom_col() +
  
  labs(x= 'State/Region', y ='Number of Species(G1)', title = 'Critically Imperiled Species by Region') +
  theme_base() +
theme(legend.position = 'none')
ggplotly(bar)  


#Let's see how many different species and their global rank by region (TX)
texas <- Caves %>% filter(Region %in% 'TX') %>% 
  group_by(G_status) %>% 
  tally %>% 
  arrange(desc(n))

ggplot(data = texas, aes(x = G_status, y = n)) +
  geom_col() +
  labs(x = 'Global Rank', y = 'Number of Species', title = 'Number of Species in each Rank') + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


#Let's try to make a function that gives species in each region rank per region 
state <- 'TX'

stat_df <- Caves %>% filter(Region == state) %>% group_by(G_status) %>% tally



ggplot(stat_df, aes(x = G_status, y = n, fill = G_status)) +
  geom_col() +
  scale_x_discrete(drop = FALSE) +
  scale_fill_manual(values = c( "G1: Critically Imperiled" ='darkred',
                                "G2: Imperiled" = 'darkorange3',
                                "G3: Vulnerable" = 'darkgoldenrod2',
                                "G4: Apparently Secure" = 'chartreuse4',
                                "G5: Secure" = 'darkgreen',
                                "GH: Possibly Extinct" = 'black',
                                "GX: Presumed Extinct" = 'darkslategray4',
                                "GNR: Unranked" = 'darkslategray',
                                "GU: Unrankable" = 'darkgrey'
                                 )) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(x = 'Global Status', y = 'Number of Species', title = "Number of Species per Rank (TX)") +
  theme(legend.position = 'none')

states <- c('AB','AK','AL','AZ','AR','BC','CA','CO','CT','DC','FL','GA',
            'HI','IA','ID','IL','IN','KS','KY','LA','MA','MD','MI','MN','MO',
            'MS','MT','NC','NM','NV','NY','OH','OK','ON','OR','PA','QC','SC',
            'SD','TN','TX','VA','VT','WA','WI','WV','WY')
reg <-Caves %>%  group_by(Region) %>% 
  tally


#Here is the actual function that should save to my files

for(state in states){
  
  stat_df <- Caves %>% filter(Region == state) %>% group_by(G_status) %>% tally
  
  
  (ggplot(stat_df, aes(x = G_status, y = n, fill = G_status)) +
    geom_col() +
    scale_x_discrete(drop = FALSE) +
    scale_fill_manual(values = c( "G1: Critically Imperiled" ='darkred',
                                  "G2: Imperiled" = 'darkorange3',
                                  "G3: Vulnerable" = 'darkgoldenrod2',
                                  "G4: Apparently Secure" = 'chartreuse4',
                                  "G5: Secure" = 'darkgreen',
                                  "GH: Possibly Extinct" = 'black',
                                  "GX: Presumed Extinct" = 'darkslategray4',
                                  "GNR: Unranked" = 'darkslategray',
                                  "GU: Unrankable" = 'darkgrey'
    )) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    labs(x = 'Global Status', y = 'Number of Species', title = state) +
    theme(legend.position = 'none'))
  
  ggsave(file = paste0(state, '.png'),
         width = 6,
         height = 4)
  
  
  
}


######################################################################

# Testing out trying to keep all the colors the same for each variable

scale_fill_manual(values = c( "G1: Critically Imperiled" ='darkred',
                              "G2: Imperiled" = 'darkorange3',
                              "G3: Vulnerable" = 'darkgoldenrod2',
                              "G4: Apparently Secure" = 'darkgreen',
                              "G5: Secure" = 'darkolivegreen',
                               "GH: Possibly Extinct" = 'black',
                              "GNR: Unranked" = 'darkslategray',
                              "GU: Unrankable" = 'darkgrey',
                              "GX: Presumed Extinct" = 'darkslategray4' ))

########################################################################

#Trying to make a new variable for the map that includes Region, total species, % in each global status

percent_status <- Caves %>% group_by(G_status,Region ) %>% 
  tally %>% 
  arrange(desc(n))







#Playing around with Maps 

library(usmap)

plot_usmap()




########################################################################

# Pie Charts 

slices <- c(3144, 3249, 4906, 8504, 17305)
lbls <- c("G1", "G2", "G3", "G4", "G5")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls,
    main="All Species in Nature Serve(Excluding Cave)") + 
  scale_fill_manual(values = c('darkred','darkorange3',
                                'darkgoldenrod2',
                                'darkgreen',
                                'darkolivegreen'))
  
# Another way

# Get the positions
All_data2 <- All_data %>% 
  mutate(csum = rev(cumsum(rev(pct))), 
         pos = pct/2 + lead(csum, 1),
         pos = if_else(is.na(pos), pct/2, pos))

ggplot(All_data, aes(x = "" , y = pct, fill = fct_inorder(G_status))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c('darkred','darkorange3',
                               'darkgoldenrod2',
                               'chartreuse4',
                               'darkgreen')) +
  geom_label_repel(data = All_data2,
                   aes(y = pos, label = paste0(pct, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  labs(title = "All Non-Cave Species in NatureServe") +
  guides(fill = guide_legend(title = "Global Status")) +
  theme_void()

All_data <- All_data %>% mutate(pct = round((n/37552)*100))

#So now lets do one with cave species 

small_cave2 <- small_cave %>% 
  mutate(csum = rev(cumsum(rev(pct))), 
         pos = pct/2 + lead(csum, 1),
         pos = if_else(is.na(pos), pct/2, pos))

ggplot(small_cave, aes(x = "" , y = pct, fill = fct_inorder(G_status))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c('darkred','darkorange3',
                               'darkgoldenrod2',
                               'chartreuse4',
                               'darkgreen')) +
  geom_label_repel(data = small_cave2,
                   aes(y = pos, label = paste0(pct, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  labs(title = "Cave Species in NatureServe (G1-G5, 93%)") +
  guides(fill = guide_legend(title = "Global Status")) +
  theme_void()

small_cave <- small_cave %>% mutate(pct = round((n/1090)*100))
  


#######################################################################

#Ranking by Taxonomic Groups 

#By taxonomic group: arthropods G1-G5, GH+GX
#(A) hexapods (= collembola + insecta + diplura)
#(B) myriapods (= diplopoda + chilopoda)
#(C) arachnida
#(D) crustaceans (= malacostraca + maxillopoda + ostracoda)


# A This will be the Bar graph for Hexapods

Hexapods <- Caves %>% filter(Class %in% c("Collembola", "Insecta","Diplura")) %>%
  group_by(G_status) %>% distinct(Species) %>% 
  tally

Hexa <- ggplot(data = Hexapods, aes(x = G_status, y = n, fill = G_status)) +
  geom_col() +
  scale_x_discrete(drop = FALSE) +
  geom_text(aes(label = n), vjust = -0.25, colour = "black", size = 2.75) +
  ylim(0,250) +
  scale_fill_manual(values = c( "G1: Critically Imperiled" ='darkred',
                                "G2: Imperiled" = 'darkorange3',
                                "G3: Vulnerable" = 'darkgoldenrod2',
                                "G4: Apparently Secure" = 'chartreuse4',
                                "G5: Secure" = 'darkgreen',
                                "GH: Possibly Extinct" = 'black',
                                "GX: Presumed Extinct" = 'darkslategray4',
                                "GNR: Unranked" = 'darkslategray',
                                "GU: Unrankable" = 'darkgrey' )) +
  labs(x = 'Global Status', y = 'Number of Species', title = "Global Status of Hexapods",
       subtitle = "Collembola, Insecta, Diplura") +
  theme_classic()  +
  theme(legend.position = 'none') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  

#B This will be the Bar graph for the myriapods 

Myriapods <- Caves %>% filter(Class %in% c("Diplopoda", "Chilopoda")) %>% 
  group_by(G_status) %>% distinct(Species) %>% 
  tally 

Myria <- ggplot(data = Myriapods, aes(x = G_status, y = n, fill = G_status)) +
  geom_col() +
  scale_x_discrete(drop = FALSE) +
  geom_text(aes(label = n), vjust = -0.25, colour = "black", size = 2.75) +
  ylim(0,250) +
  scale_fill_manual(values = c( "G1: Critically Imperiled" ='darkred',
                                "G2: Imperiled" = 'darkorange3',
                                "G3: Vulnerable" = 'darkgoldenrod2',
                                "G4: Apparently Secure" = 'chartreuse4',
                                "G5: Secure" = 'darkgreen',
                                "GH: Possibly Extinct" = 'black',
                                "GX: Presumed Extinct" = 'darkslategray4',
                                "GNR: Unranked" = 'darkslategray',
                                "GU: Unrankable" = 'darkgrey' )) +
  labs(x = 'Global Status', y = 'Number of Species', title = "Global Status of Myriapods",
       subtitle = "Diplopoda, Chilopoda") +
  theme_classic()  +
  theme(legend.position = 'none') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


# C This will be the Bar graph for the arachnida 

Arachnida <- Caves %>% filter(Class %in% c("Arachnida")) %>% 
  group_by(G_status) %>% distinct(Species) %>% 
  tally 
  
Arach <- ggplot(data = Arachnida, aes(x = G_status, y = n, fill = G_status)) +
  geom_col() +
  scale_x_discrete(drop = FALSE) +
  geom_text(aes(label = n), vjust = -0.25, colour = "black", size = 2.75) +
  ylim(0,250) +
  scale_fill_manual(values = c( "G1: Critically Imperiled" ='darkred',
                                "G2: Imperiled" = 'darkorange3',
                                "G3: Vulnerable" = 'darkgoldenrod2',
                                "G4: Apparently Secure" = 'chartreuse4',
                                "G5: Secure" = 'darkgreen',
                                "GH: Possibly Extinct" = 'black',
                                "GX: Presumed Extinct" = 'darkslategray4',
                                "GNR: Unranked" = 'darkslategray',
                                "GU: Unrankable" = 'darkgrey' )) +
  labs(x = 'Global Status', y = 'Number of Species', title = "Global Status of Arachnida") +
  theme_classic()  +
  theme(legend.position = 'none') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# D This will be the Bar graph for the crustaceans 

Crustaceans <- Caves %>% filter(Class %in% c("Malacostraca", "Maxillopoda", "Ostracoda")) %>% 
  group_by(G_status) %>% distinct(Species) %>% 
  tally 

Crust <- ggplot(data = Crustaceans, aes(x = G_status, y = n, fill = G_status)) +
  geom_col() +
  scale_x_discrete(drop = FALSE) +
  geom_text(aes(label = n), vjust = -0.25, colour = "black", size = 2.75) +
  ylim(0,250) +
  scale_fill_manual(values = c( "G1: Critically Imperiled" ='darkred',
                                "G2: Imperiled" = 'darkorange3',
                                "G3: Vulnerable" = 'darkgoldenrod2',
                                "G4: Apparently Secure" = 'chartreuse4',
                                "G5: Secure" = 'darkgreen',
                                "GH: Possibly Extinct" = 'black',
                                "GX: Presumed Extinct" = 'darkslategray4',
                                "GNR: Unranked" = 'darkslategray',
                                "GU: Unrankable" = 'darkgrey' )) +
  labs(x = 'Global Status', y = 'Number of Species', title = "Global Status of Crustaceans",
       subtitle = "Malacostraca, Maxillopoda, Ostracoda") +
  theme_classic()  +
  theme(legend.position = 'none') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


#  By taxonomic group: everything else G1-G5, GH+GX
#(A) vertebrates (= craniata)
#(B) gastropoda
#(C) platyhelminthes
#(D) annelida

# A Vertebrates 
Vertebrates <- Caves %>% filter(Phylum %in% "Craniata") %>% 
  group_by(G_status) %>% distinct(Species) %>% 
  tally 

ggplot(data = Vertebrates, aes(x = G_status, y = n, fill = G_status)) +
  geom_col() +
  scale_x_discrete(drop = FALSE) +
  geom_text(aes(label = n), vjust = -0.25, colour = "black", size = 2.75) +
  ylim(0,25) +
  scale_fill_manual(values = c( "G1: Critically Imperiled" ='darkred',
                                "G2: Imperiled" = 'darkorange3',
                                "G3: Vulnerable" = 'darkgoldenrod2',
                                "G4: Apparently Secure" = 'chartreuse4',
                                "G5: Secure" = 'darkgreen',
                                "GH: Possibly Extinct" = 'black',
                                "GX: Presumed Extinct" = 'darkslategray4',
                                "GNR: Unranked" = 'darkslategray',
                                "GU: Unrankable" = 'darkgrey' )) +
  labs(x = 'Global Status', y = 'Number of Species', title = "Global Status of Vertebrates") +
  theme_classic()  +
  theme(legend.position = 'none') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
#B Gastropoda
Gastropoda <- Caves %>% filter(Class %in% "Gastropoda") %>% 
  group_by(G_status) %>% distinct(Species) %>% 
  tally 

ggplot(data = Gastropoda, aes(x = G_status, y = n, fill = G_status)) +
  geom_col() +
  scale_x_discrete(drop = FALSE) +
  geom_text(aes(label = n), vjust = -0.25, colour = "black", size = 2.75) +
  ylim(0,25) +
  scale_fill_manual(values = c( "G1: Critically Imperiled" ='darkred',
                                "G2: Imperiled" = 'darkorange3',
                                "G3: Vulnerable" = 'darkgoldenrod2',
                                "G4: Apparently Secure" = 'chartreuse4',
                                "G5: Secure" = 'darkgreen',
                                "GH: Possibly Extinct" = 'black',
                                "GX: Presumed Extinct" = 'darkslategray4',
                                "GNR: Unranked" = 'darkslategray',
                                "GU: Unrankable" = 'darkgrey' )) +
  labs(x = 'Global Status', y = 'Number of Species', title = "Global Status of Gastropoda") +
  theme_classic()  +
  theme(legend.position = 'none') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
#C Platyhelminthes
Platyhelminthes <- Caves %>% filter(Phylum %in% "Platyhelminthes") %>% 
  group_by(G_status) %>% distinct(Species) %>% 
  tally 

ggplot(data = Platyhelminthes, aes(x = G_status, y = n, fill = G_status)) +
  geom_col() +
  scale_x_discrete(drop = FALSE) +
  geom_text(aes(label = n), vjust = -0.25, colour = "black", size = 2.75) +
  ylim(0,25) +
  scale_fill_manual(values = c( "G1: Critically Imperiled" ='darkred',
                                "G2: Imperiled" = 'darkorange3',
                                "G3: Vulnerable" = 'darkgoldenrod2',
                                "G4: Apparently Secure" = 'chartreuse4',
                                "G5: Secure" = 'darkgreen',
                                "GH: Possibly Extinct" = 'black',
                                "GX: Presumed Extinct" = 'darkslategray4',
                                "GNR: Unranked" = 'darkslategray',
                                "GU: Unrankable" = 'darkgrey' )) +
  labs(x = 'Global Status', y = 'Number of Species', title = "Global Status of Platyhelminthes") +
  theme_classic()  +
  theme(legend.position = 'none') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
#D Annelida 
Annelida <- Caves %>% filter(Phylum %in% "Annelida") %>% 
  group_by(G_status) %>% distinct(Species) %>% 
  tally 

ggplot(data = Annelida, aes(x = G_status, y = n, fill = G_status)) +
  geom_col() +
  scale_x_discrete(drop = FALSE) +
  geom_text(aes(label = n), vjust = -0.25, colour = "black", size = 2.75) +
  ylim(0,25) +
  scale_fill_manual(values = c( "G1: Critically Imperiled" ='darkred',
                                "G2: Imperiled" = 'darkorange3',
                                "G3: Vulnerable" = 'darkgoldenrod2',
                                "G4: Apparently Secure" = 'chartreuse4',
                                "G5: Secure" = 'darkgreen',
                                "GH: Possibly Extinct" = 'black',
                                "GX: Presumed Extinct" = 'darkslategray4',
                                "GNR: Unranked" = 'darkslategray',
                                "GU: Unrankable" = 'darkgrey' )) +
  labs(x = 'Global Status', y = 'Number of Species', title = "Global Status of Annelida") +
  theme_classic()  +
  theme(legend.position = 'none') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
