library(tidyverse)
library(readxl)
library(networktools)
library(igraph)
library(dplyr) 
library(bibliometrix)

data<-read_excel("submissions_2020114_20220114.xlsx") 
collegedata <- read_excel("vsu_colleges_depts.xlsx")
names(data)=c("title","purpose","pi","agency","requested","idc","inkind","submitted","status","collab_code","collab_type","external","team")
#duplicating dept and college columns

#collegedata1 <- collegedata %>% 
 # mutate(dept2 = dept) %>% 
  #mutate(college2 = college)

data_collab<-data %>%
  filter(str_count(team, " - ")>1) %>%
  mutate(member=str_split(team, "; ")) 

data_solo<-data %>%
  filter(str_count(team, " - ")<=1)

final<-data.frame(matrix(ncol=15, nrow=0))
colnames(final)<-c("title", "purpose", "pi", "agency", "requested", "idc",   "inkind", "submitted", "status", "collab_code", "collab_type", "external", "team",  "team1", "team2")

for (i in 1:nrow(data_collab)) {
row_filter<-data_collab %>%
  filter(row_number()==i)
bg<-row_filter %>% select(title:team)
team_members<-row_filter %>% select(-c(title:team)) 
team_members_sep<-matrix(unlist(team_members))
team_pairs<-combn(team_members_sep,2, simplify=T) %>%
  t()%>%
  as.data.frame() %>%
  rename(member1= V1,
         member2= V2)
reunite<-cbind(bg, team_pairs)
final<-rbind(final, reunite)}

final<-final %>%
  mutate(member1=str_replace(member1,";", ""))%>%
  mutate(member2=str_replace(member2,";", ""))%>%
  separate(member1, into=c("fac1", "dept1"), sep=" - ", extra="drop") %>%
  separate(member2, into=c("fac2", "dept2"), sep=" - ", extra="drop") 

#put department names for missing names
final$dept1 <- ifelse(final$fac1 == "Wanda Velez", "Biology", final$dept1) 
final$dept2 <- ifelse(final$fac2 == "Wanda Velez", "Biology", final$dept2) 

final$dept1 <- ifelse(final$fac1 == "Xueping Liang", "Computer Information Systems", final$dept1) 
final$dept2 <- ifelse(final$fac2 == "Xueping Liang", "Computer Information Systems", final$dept2)


#drop na rows 
final <- final[final$dept1 != "", ]

#join data 
final_college1 <- final %>% 
  left_join(collegedata, by = c("dept1" = "dept"))  

collegedata2 <- collegedata %>% 
  rename_at("college", ~"college2") %>% 
  rename_at("dept",~ "dept2")
final_college2 <- final_college1 %>% 
  left_join(collegedata2, by = c("dept2" = "dept2"))

#change the order of columns  
final_college2 <- final_college2 %>% 
  relocate(college, .before = fac2)

#put college names for rows with NA 
final_college2$college <- ifelse(final_college2$dept1 == "Office of the Provost", "No College", final_college2$college) 
final_college2$college2 <- ifelse(final_college2$dept2 == "Office of the Provost", "No College", final_college2$college2) 

final_college2$college <- ifelse(final_college2$dept1 == "Distance Education", "No College", final_college2$college) 
final_college2$college2 <- ifelse(final_college2$dept2 == "Distance Education", "No College", final_college2$college2)

final_college2$college <- ifelse(final_college2$dept1 == "Planning and Institutional Effectiveness", "No College", final_college2$college) 
final_college2$college2 <- ifelse(final_college2$dept2 == "Planning and Institutional Effectiveness", "No College", final_college2$college2)

final_college2$college <- ifelse(final_college2$dept1 == "Research, Economic Development, and Graduate Studies", "No College", final_college2$college) 
final_college2$college2 <- ifelse(final_college2$dept2 == "Research, Economic Development, and Graduate Studies", "No College", final_college2$college2) 

finalcollege3 <- graph_from_data_frame(final_college2)

data_solo<-data_solo %>%
  mutate(member1=team,
         member2="")%>%
  mutate(member1=str_replace(member1,";", "")) %>%
  separate(member1, into=c("fac1", "dept1"), sep=" - ", extra="drop") %>%
  separate(member2, into=c("fac2", "dept2"), sep=" - ", extra="drop")

collab_table<-rbind(final, data_solo)

collab_pairs<-select(final, fac1, fac2)


collab_single<-graph_from_data_frame(collab_pairs, directed=F)

collab_single_coms<-cluster_louvain(collab_single)
V(collab_single)$community<-collab_single_coms$membership
#E(collab3)$degree<-degree(collab3)
plot(collab_single,
     vertex.color=V(collab_single)$community,
     edge.width=degree(collab_single),
     edge.color="blue")

plot(collab_single,
     vertex.color=V(collab_single)$community,
     edge.width=degree(collab_single),
     edge.color="blue",
     layout=layout.circle(collab_single))

 
collab_match<-graph_from_data_frame(interaction_counts, directed=F) 
collab_match_coms<-cluster_louvain(collab_match)
V(collab_match)$community<-collab_match_coms$membership

pdf("draft_full_network.pdf", width=10, height=10)  
plot(collab_match,
     vertex.color=V(collab_match)$community,
     edge.width=interaction_counts$int,
     edge.color="blue")

plot(collab_match,
     vertex.color=V(collab_match)$community,
     edge.width=interaction_counts$int,
     edge.color="blue",
     layout=layout.circle(collab_match))  

#graph for college 

fac_counts <- final_college2 %>%  
  select( fac1, fac2, college, college2) %>%
  group_by(college, college2)%>%
  summarize(int = n())


collab1 <-graph_from_data_frame(fac_counts, directed=F) 
collab2 <-cluster_louvain(collab1)
V(collab1)$community<-collab2$membership

plot(collab1,
     vertex.color=V(collab1)$community,
     edge.width=fac_counts$int,
     edge.color="blue")

plot(collab1,
     vertex.color=V(collab1)$community,
     edge.width=fac_counts$int,
     edge.color="blue",
     layout=layout.circle(collab1))   

#example bibliometrix for college
fac_counts <- final_college2 %>%  
  select( fac1, fac2, college, college2, title, submitted) %>%
  group_by(college, college2)%>%
  summarize(int = n()) 

biblio_data <- convert2df(fac_counts) 

#Perform bibliometric analysis 
result <- bibliometrix(biblio_data) 

summary(result) 

#example bibliometrix for department 
#graph for departments 
interaction_counts<-select(final_college2, fac1, fac2, college, college2, title, submitted) %>%
  group_by(fac1, fac2)%>%
  summarize(int=n())

biblio_data <- convert2df(interaction_counts) 

result <- bibliometric(biblio_data) 

summary(result)

#graph showing department(color) and college(shape), size of a node 


#giving each college a shape 
colleges <- unique(finalcollege3$college) 


shapes <- c("circle", "square", "triangle", "diamond", "pentagon", "octagon, rectangle", "oval") 

#initializes empty shape map
college_shapes <- character() 

for(i in seq_along(colleges)){ 
  college_shapes[colleges[i]] <- shapes[i] 
} 

#giving each department a color 
departments <- unique(finalcollege$dept1) 
 
color <- c("red", "blue", "purple", "honeydew3", "grey","orange","white","brown","pink","yellow","maroon","turquoise","tan","cyan","navy","gold","tomato","violet","salmon","chocolate", "darkgoldenrod4", "cornflowerblue", "firebrick" )

dept_colors <- character() 

for(i in seq_along(departments)){ 
  dept_colors[departments[i]] <- color[i] 
    } 
 
#size 
V(finalcollege)$size <- degree(finalcollege)
V(finalcollege)$color <- dept_colors[as.character(finalcollege$dept1)]
V(finalcollege)$shape <- college_shapes[as.character(finalcollege$college)]

plot(finalcollege,
     vertex.color=V(finalcollege)$color,
     vertex.size = V(finalcollege)$size, 
     vertex.shape = V(finalcollege)$shape,
     edge.color="black")







