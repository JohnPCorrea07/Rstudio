rm(list = ls())
#install.packages("readxl")
#install.packages("tidyverse")
#install.packages("reshape")
#install.packages("reshape2")
#install.packages("lubridate")
#install.packages("pryr")
#install.packages("scales")
#install.packages("devtools")
#devtools::install_github("gadenbuie/regexplain")
#install.packages("xlsx")


library(reshape2)       # some packages to manipulate data
library(tidyverse)       # some packages to manipulate data
library(readxl)          # import from excel
library(lubridate)
library(pryr)    #check memory use for large databases
library(scales) # normalize or standartized - standard deviation
library(xlsx)       # some packages to manipulate data

##################################
########  DATA TREATMENT  ########
##################################

####### importing 3 DATA SOURCES: 1-INV, 2- TRANSACT AND 3-SALES #########

# table 1- registering in INV Stores + CDI
inv <- read_excel("C:/Users/Juliano/Dropbox/Cooxupe/4_Planilhas_extraidas/Projeto_2833/1 CADASTRO ITEM - Validação CDI.xlsx")
inv = as.data.frame(inv)

# table 2- transactions (2018, 2019, 2020) all stores + CDI (2018, 2019, 2020)

# SHEET 2018 of the file "2 Item_estoque - Validação CDI(J48,44,45)" has duplicated items - Sent another file with this data
#transac2018cdi<- read_excel("C:/Users/Juliano/Dropbox/Cooxupe/4_Planilhas_extraidas/Projeto_2833/2 Item_estoque - Validação CDI(J48,44,45).xlsx", sheet  = "2018")
# eliminating duplicated lines from the sheet 2018
#transac2018cdi <- transac2018cdi %>% distinct(ORG, ITEM, DESCRICAO_ITEM, DATA_TRANSACAO,TIPO_TRANSACAO)
#rm(t2018, t2018cdi,t2019,t2019cdi,t2020,t2020cdi)

transac2018<- read_excel("C:/Users/Juliano/Dropbox/Cooxupe/4_Planilhas_extraidas/Projeto_2833/2 Item_estoque - Validação CDI.xlsx", sheet  = "2018")
transac2019<- read_excel("C:/Users/Juliano/Dropbox/Cooxupe/4_Planilhas_extraidas/Projeto_2833/2 Item_estoque - Validação CDI.xlsx", sheet  = "2019")
transac2020<- read_excel("C:/Users/Juliano/Dropbox/Cooxupe/4_Planilhas_extraidas/Projeto_2833/2 Item_estoque - Validação CDI.xlsx", sheet  = "2020")
transac2018cdi<- read_excel("C:/Users/Juliano/Dropbox/Cooxupe/4_Planilhas_extraidas/Projeto_2833/2 Item_estoque - Validação CDI(J48,44,45)2018.xlsx", sheet  = "2018")
transac2019cdi<- read_excel("C:/Users/Juliano/Dropbox/Cooxupe/4_Planilhas_extraidas/Projeto_2833/2 Item_estoque - Validação CDI(J48,44,45).xlsx", sheet  = "2019")
transac2020cdi<- read_excel("C:/Users/Juliano/Dropbox/Cooxupe/4_Planilhas_extraidas/Projeto_2833/2 Item_estoque - Validação CDI(J48,44,45).xlsx", sheet  = "2020")
transac2018 = as.data.frame(transac2018)
transac2019 = as.data.frame(transac2019)
transac2020 = as.data.frame(transac2020)
transac2018cdi = as.data.frame(transac2018cdi)
transac2019cdi = as.data.frame(transac2019cdi)
transac2020cdi = as.data.frame(transac2020cdi)

class(transac2019)
#### checking duplicated lines####
#t2018 <- transac2018 %>% group_by (ORG, ITEM, DESCRICAO_ITEM, DATA_TRANSACAO,TIPO_TRANSACAO) %>% filter (n()>1)
#t2018cdi <- transac2018cdi %>% group_by (ORG, ITEM, DESCRICAO_ITEM, DATA_TRANSACAO,TIPO_TRANSACAO) %>% filter (n()>1)
#t2019 <- transac2019 %>% group_by (ORG, ITEM, DESCRICAO_ITEM, DATA_TRANSACAO,TIPO_TRANSACAO) %>% filter (n()>1)
#t2019cdi <- transac2019cdi %>% group_by (ORG, ITEM, DESCRICAO_ITEM, DATA_TRANSACAO,TIPO_TRANSACAO) %>% filter (n()>1)
#t2020 <- transac2020 %>% group_by (ORG, ITEM, DESCRICAO_ITEM, DATA_TRANSACAO,TIPO_TRANSACAO) %>% filter (n()>1)
#t2020cdi <- transac2020cdi %>% group_by (ORG, ITEM, DESCRICAO_ITEM, DATA_TRANSACAO,TIPO_TRANSACAO) %>% filter (n()>1)
#rm(t2018,t2018cdi,t2019, t2019cdi, t2020, t2020cdi)

# table 3- sales all stores + CDI
temp_sales1 <- read_excel("C:/Users/Juliano/Dropbox/Cooxupe/4_Planilhas_extraidas/Projeto_2833/3 Estoque_saida - Validação CDI.xlsx")
temp_sales2 <- read_excel("C:/Users/Juliano/Dropbox/Cooxupe/4_Planilhas_extraidas/Projeto_2833/3 Estoque_saida - Validação CDI(J48,44,45).xlsx")
sales<-bind_rows(temp_sales1,temp_sales2)  # bind_rows replaces combine command 'R> sales<-combine(temp_sales1,temp_sales2)'
rm(temp_sales1)   #delete data frame
rm(temp_sales2)   #delete data frame
sales = as.data.frame(sales)
class(sales)

# table 4- custo dos items
new_cost <- read_excel("C:/Users/Juliano/Dropbox/Cooxupe/4_Planilhas_extraidas/Projeto_2833/4-Max_Valor_Custo.xlsx", sheet  = "SQL Results")
new_cost = as.data.frame(new_cost)
class(new_cost)

#### DATA-FRAME 3 - SALES #####

#converting ITEM variable character to numeric (inv and sales data frames)
sapply(sales, class)                                      #see if variables are numeric, string..
sapply(sales, mode)                                       #see if variables are numeric, string..
sales$ITEM <- as.numeric(sales$ITEM)                      #convert ITEM from character to numeric and rename to item

#inverting the signal of Quantity and renaming variables
sales$QUANTIDADE = sales$QUANTIDADE * -1                  #create a new variable qty inverting the signal
sales <- sales %>% rename(mth_sold = DATA_TRANSACAO)      #Renaming variable DATA_TRANSACAO
sales <- sales %>% rename(qty = QUANTIDADE)               #Renaming variable QUANTIDADE
sales <- sales %>% rename(sales_description = DESCRICAO_ITEM)   #Renaming variable DESCRICAO


#R consider the - as a subtraction operator "when" within a variable name.
#This not works when counting, summarizing...the option is replace varable_name from 01-2018 to jan2018
sales$mth_sold <- sapply(sales$mth_sold,gsub,pattern="01-",replacement="Jan")
sales$mth_sold <- sapply(sales$mth_sold,gsub,pattern="02-",replacement="Feb")
sales$mth_sold <- sapply(sales$mth_sold,gsub,pattern="03-",replacement="Mar")
sales$mth_sold <- sapply(sales$mth_sold,gsub,pattern="04-",replacement="Apr")
sales$mth_sold <- sapply(sales$mth_sold,gsub,pattern="05-",replacement="May")
sales$mth_sold <- sapply(sales$mth_sold,gsub,pattern="06-",replacement="Jun")
sales$mth_sold <- sapply(sales$mth_sold,gsub,pattern="07-",replacement="Jul")
sales$mth_sold <- sapply(sales$mth_sold,gsub,pattern="08-",replacement="Aug")
sales$mth_sold <- sapply(sales$mth_sold,gsub,pattern="09-",replacement="Sep")
sales$mth_sold <- sapply(sales$mth_sold,gsub,pattern="10-",replacement="Oct")
sales$mth_sold <- sapply(sales$mth_sold,gsub,pattern="11-",replacement="Nov")
sales$mth_sold <- sapply(sales$mth_sold,gsub,pattern="12-",replacement="Dec")


#moving data-frame from the format LONG to WIDE
sales_wide <- dcast(sales, ORG + ITEM + sales_description ~ mth_sold, value.var="qty")

#relocate order in sales data-frame
sales_wide <- sales_wide %>% relocate(Jan2018, Feb2018, Mar2018, Apr2018, May2018, Jun2018, Jul2018, Aug2018, Sep2018, Oct2018, Nov2018, Dec2018,
                                      Jan2019, Feb2019, Mar2019, Apr2019, May2019, Jun2019, Jul2019, Aug2019, Sep2019, Oct2019, Nov2019, Dec2019,
                                      Jan2020, Feb2020, Mar2020, Apr2020, May2020, Jun2020, .after = sales_description)

#just to check
head(sales$qty)
head(sales_wide$Jan2018)

rm(sales)

####### DATA-FRAME 1 - INV ######

sapply(inv, class)                                      #see if variables are numeric, string..
sapply(inv, mode)                                       #see if variables are numeric, string..
inv$ITEM <- as.numeric(inv$DM_ITEM_CODE)                #convert ITEM from character to numeric and rename to item
inv <- inv %>% relocate(ITEM, .after = Organizacao)     #Relocate ITEM as 2nd column
inv <- inv %>% rename(ORG = Organizacao)                #Relocate Organizacao to ORG

# removing the ORGS out-of-scope (w Tidyverse)
inv <- inv %>% filter(ORG %in% c("L01","L03","L04","L05","L06","L07","L08","L09","L10","L11","L12",
                                         "L14","L15","L16","L18","L19","L20","076","300","301","302","303",
                                         "306","307","308","309","310","311","312","314","F44","F45","J48"))

# removing not used columns in the INV dataframe with Tidyverse
inv <- inv %>% select(ORG, ITEM, ITEM_DESC, CATEGORY_DESC, GROUP_DESC, LINE_DESC, SUBLINE_DESC, Preco, CUSTO_ITEM, INVENTORY_PLANNING_CODE, MRP_SAFETY_STOCK_CODE, POSTPROCESSING_LEAD_TIME, PREPROCESSING_LEAD_TIME, MINIMUM_ORDER_QUANTITY, MAXIMUM_ORDER_QUANTITY, FIXED_LOT_MULTIPLIER, FIXED_ORDER_QUANTITY, FULL_LEAD_TIME)


#### DATA-FRAME 2 - TRANSACTION #####

transac2018$ITEM    <- as.numeric(transac2018$ITEM)                      #convert ITEM from character to numeric and rename to item
transac2019$ITEM    <- as.numeric(transac2019$ITEM)                      #convert ITEM from character to numeric and rename to item
transac2020$ITEM    <- as.numeric(transac2020$ITEM)                      #convert ITEM from character to numeric and rename to item
transac2018cdi$ITEM <- as.numeric(transac2018cdi$ITEM)                  #convert ITEM from character to numeric and rename to item
transac2019cdi$ITEM <- as.numeric(transac2019cdi$ITEM)                  #convert ITEM from character to numeric and rename to item
transac2020cdi$ITEM <- as.numeric(transac2020cdi$ITEM)                  #convert ITEM from character to numeric and rename to item

temp_2018 <- full_join(transac2018, transac2018cdi)
temp_2019 <- full_join(transac2019, transac2019cdi)
temp_2020 <- full_join(transac2020, transac2020cdi)
temp_18_19 <- full_join(temp_2018, temp_2019)
transac    <- full_join(temp_18_19, temp_2020)

transac$DATA_TRANSACAO <- as.Date(transac$DATA_TRANSACAO)     #convert ITEM from character to numeric and rename to item
class(transac)

rm(temp_18_19)
rm(transac2018, transac2019, transac2020)
rm(temp_2018, temp_2019, temp_2020)
rm(transac2018cdi, transac2019cdi, transac2020cdi)

#create a table only with the on hand at the end of each month Dec2017-Jun2020
av_stock  <- transac %>% filter(TIPO_TRANSACAO == "saldo_inicial")
av_stock <- av_stock %>% rename(transac_description = DESCRICAO_ITEM)      #Renaming variable DATA_TRANSACAO
av_stock <- av_stock %>% rename(dt_transaction = DATA_TRANSACAO)      #Renaming variable DATA_TRANSACAO
av_stock <- av_stock %>% rename(qty = QUANTIDADE)      #Renaming variable DATA_TRANSACAO
av_stock  <- av_stock %>% select(ORG, ITEM,dt_transaction, transac_description, qty)
av_stock$dt_transaction <- format(av_stock$dt_transaction, "%h%Y")

#including _stk in the month to be different from sold_month
av_stock$dt_transaction <- sapply(av_stock$dt_transaction,gsub,pattern=2020,replacement="2020_stk")
av_stock$dt_transaction <- sapply(av_stock$dt_transaction,gsub,pattern=2019,replacement="2019_stk")
av_stock$dt_transaction <- sapply(av_stock$dt_transaction,gsub,pattern=2018,replacement="2018_stk")
av_stock$dt_transaction <- sapply(av_stock$dt_transaction,gsub,pattern=2017,replacement="2017_stk")

class(av_stock)
rm(transac)  # delete the joined transac temp table
#av_stock<-av_stock %>% distinct(ORG,ITEM,dt_transaction, .keep_all = TRUE)

#moving data-frame from the format LONG to WIDE
av_stock_wide  <- dcast(av_stock, ORG + ITEM + transac_description ~ dt_transaction, value.var="qty")

#relocate order in sales data-frame
av_stock_wide <- av_stock_wide %>% relocate(Dec2017_stk, Jan2018_stk, Feb2018_stk, Mar2018_stk, Apr2018_stk, May2018_stk, Jun2018_stk, Jul2018_stk, Aug2018_stk, Sep2018_stk, Oct2018_stk, Nov2018_stk, Dec2018_stk,
                                      Jan2019_stk, Feb2019_stk, Mar2019_stk, Apr2019_stk, May2019_stk, Jun2019_stk, Jul2019_stk, Aug2019_stk, Sep2019_stk, Oct2019_stk, Nov2019_stk, Dec2019_stk,
                                      Jan2020_stk, Feb2020_stk, Mar2020_stk, Apr2020_stk, May2020_stk, .after = transac_description)


rm(av_stock)


#### DATA-FRAME 4 - NEW-COST #####

new_cost$ITEM    <- as.numeric(new_cost$DM_ITEM_CODE)         #convert ITEM from character to numeric and rename to item
new_cost  <- new_cost %>% relocate(ITEM)
new_cost <- select(new_cost, -2,-3)                           #delete column 2 AND 3
new_cost <- new_cost %>% rename(NOVO_CUSTO = CUSTO_ITEM)      #Renaming variable DATA_TRANSACAO



###################################################
#### JOIN DATA FRAMES sales and inv ##############
###################################################

mem_used()

#Joining data frames 1 and 3 - inv and sales
cooxupe_df <- left_join(inv, sales_wide)
class(cooxupe_df)

#creating new variable store_type (nucleo, loja or uremota) 
cooxupe_df$store_type  = ifelse(cooxupe_df$ORG %in% c("L03","L04","L07","L09","L10","L11","L12","L14","L15","L18", "L19"),"núcleo",
                                ifelse(cooxupe_df$ORG %in% c("L01","L05","L06","L08","L16","L20","076"),"loja",
                                       ifelse(cooxupe_df$ORG %in% c("J48", "F44", "F45"), "CDI","uremota")))
rm(sales_wide)

#relocating columns 
cooxupe_df <- cooxupe_df %>% rename(inv_description = ITEM_DESC)                        #Renaming variable QUANTIDADE
cooxupe_df <- cooxupe_df %>% relocate(store_type, .after = inv_description)           #Relocate store_type as 3rd column

rm(inv)


#Joining data frame 2 - transactions
cooxupe_df <- left_join(cooxupe_df, av_stock_wide)

#REPLACING NA PER 0
cooxupe_df <- cooxupe_df %>% replace_na(list(Dec2017_stk=0, Jan2018_stk=0, Feb2018_stk=0, Mar2018_stk=0, Apr2018_stk=0, May2018_stk=0, Jun2018_stk=0, Jul2018_stk=0, Aug2018_stk=0, Sep2018_stk=0, Oct2018_stk=0, Nov2018_stk=0, Dec2018_stk=0,
                                             Jan2019_stk=0, Feb2019_stk=0, Mar2019_stk=0, Apr2019_stk=0, May2019_stk=0, Jun2019_stk=0, Jul2019_stk=0, Aug2019_stk=0, Sep2019_stk=0, Oct2019_stk=0, Nov2019_stk=0, Dec2019_stk=0,
                                             Jan2020_stk=0, Feb2020_stk=0, Mar2020_stk=0, Apr2020_stk=0, May2020_stk=0))

cooxupe_df <- cooxupe_df %>% replace_na(list(Jan2018=0, Feb2018=0, Mar2018=0, Apr2018=0, May2018=0, Jun2018=0, Jul2018=0, Aug2018=0, Sep2018=0, Oct2018=0, Nov2018=0, Dec2018=0,
                                             Jan2019=0, Feb2019=0, Mar2019=0, Apr2019=0, May2019=0, Jun2019=0, Jul2019=0, Aug2019=0, Sep2019=0, Oct2019=0, Nov2019=0, Dec2019=0,
                                             Jan2020=0, Feb2020=0, Mar2020=0, Apr2020=0, May2020=0, Jun2020=0))
rm(av_stock_wide)

mem_used()

#
#Joining data frame 4 - NEW COST
cooxupe_df <- left_join(cooxupe_df, new_cost, by = "ITEM")
cooxupe_df <- cooxupe_df %>% relocate(NOVO_CUSTO, .after = CUSTO_ITEM)

rm(new_cost)

#summarizing sales new variable - sold_last30, sold_last12 and stock last30 and last12
cooxupe_df <- cooxupe_df %>% mutate(SO_last30 = rowSums(.[22:51],na.rm=TRUE)) %>%
  mutate (sold_last30 = if_else(SO_last30 == 0,"NO","YES"))

cooxupe_df <-cooxupe_df %>% mutate(SO_last12 = rowSums(.[40:51],na.rm=TRUE)) %>%
  mutate (sold_last12 = if_else(SO_last12 == 0,"NO","YES"))

cooxupe_df <- cooxupe_df %>% mutate(stock_last30 = rowSums(.[53:82],na.rm=TRUE)) %>%
  mutate (stk_last30 = if_else(stock_last30 == 0,"NO","YES"))

cooxupe_df <- cooxupe_df %>% mutate(stock_last12 = rowSums(.[71:82],na.rm=TRUE)) %>%
  mutate (stk_last12 = if_else(stock_last12 == 0,"NO","YES"))

cooxupe_df <- cooxupe_df %>% rename(PROCESSING_LEAD_TIME = FULL_LEAD_TIME ) 

##########  FILTERING OBSOLETE ITEMS   ##################
obsoletos <- read_excel("C:/Users/Juliano/Dropbox/Cooxupe/4_Planilhas_extraidas/Cooxupe - Relação Itens Revenda 20.08.2020.xlsx", sheet  = "Planilha1")
obsoletos %>% tally ()
obsoletos %>% tally (`Projeto?`=="Sim")
obsoletos <- obsoletos %>% filter (`Projeto?`=="Sim") 
obsoletos %>% tally ()
duplicados <- obsoletos %>% group_by (ITEM) %>% filter (n()>1)
obsoletos <- obsoletos %>% distinct(ITEM)
rm(duplicados)
cooxupe_df <- semi_join(cooxupe_df, obsoletos)
rm(obsoletos)

obsoletos1 <- read_excel("C:/Users/Juliano/Dropbox/Cooxupe/4_Planilhas_extraidas/Cooxupe - Relação Itens Revenda 31.08.2020_JC.xlsx", sheet  = "Projeto")
obsoletos1 %>% tally ()
obsoletos1 %>% tally (`Categorias`!="Fora do Projeto")
obsoletos1 <- obsoletos1 %>% filter (`Categorias`!="Fora do Projeto") 
obsoletos1 %>% tally ()
obsoletos1 <- obsoletos1 %>% rename(ITEM = `Item`)
obsoletos1 <- obsoletos1 %>% select(ITEM, `Controle Expiração`,`Categorias`)
cooxupe_df <- inner_join(cooxupe_df, obsoletos1)


####create list of Items for the project
#item_list <- cooxupe_df %>% select (ITEM, inv_description) %>% distinct(ITEM,.keep_all = TRUE)  
#item_list <- left_join(item_list,obsoletos1)
#L07_list <- cooxupe_df %>% filter(ORG=="L07")  
#difference <- anti_join(item_list, L07_list)
#write_excel_csv(difference,"C:/Users/Juliano/Dropbox/Cooxupe/6_Planilhas_geradas/trash/diferenca.csv", na = "NA", append = FALSE, delim = ",", quote_escape = "double")
#rm(item_list, L07_list, difference)

rm(obsoletos1)

##additional out-of-scope ITEMS defined manually at the end of the project MODEL1(MIN-MAX)  ##################
temp1 <- read_excel("C:/Users/Juliano/Dropbox/Cooxupe/4_Planilhas_extraidas/Cópia de Itens Zerados UAs e Lojas.xlsx", sheet  = "UAs Manual")
temp2 <- read_excel("C:/Users/Juliano/Dropbox/Cooxupe/4_Planilhas_extraidas/Cópia de Itens Zerados UAs e Lojas.xlsx", sheet  = "Lojas Manual")
temp1 <- temp1 %>% filter (Obs.=="Fora do projeto") %>% select(ITEM.x)
temp2 <- temp2 %>% filter (Obs.=="Fora do projeto") %>% select(ITEM.x)
temp1 <- temp1 %>% rename(ITEM = ITEM.x)
temp2 <- temp2 %>% rename(ITEM = ITEM.x)
cooxupe_df <- anti_join(cooxupe_df, temp1)
cooxupe_df <- anti_join(cooxupe_df, temp2)
rm(temp1, temp2)

##additional out-of-scope ITEMS defined manually at the end of the project MODEL2  ##################
temp1 <- read_excel("C:/Users/Juliano/Dropbox/Cooxupe/6_Planilhas_geradas/atualiz_manual_cooxupe/Cópia de strat_CDI_J48.xlsx", sheet  = "sem histórico - manual")
temp2 <- read_excel("C:/Users/Juliano/Dropbox/Cooxupe/6_Planilhas_geradas/atualiz_manual_cooxupe/Cópia de strat_F45.xlsx", sheet  = "com histórico")
temp3 <- read_excel("C:/Users/Juliano/Dropbox/Cooxupe/6_Planilhas_geradas/atualiz_manual_cooxupe/Cópia de strat_F45.xlsx", sheet  = "sem histórico - manual")
temp1 <- temp1 %>% filter (Categorias=="Inativar") %>% select(ITEM)
temp2 <- temp2 %>% filter (`Fora de Linha`=="Sim") %>% select(ITEM)
temp3 <- temp3 %>% filter (Observação=="Inativar") %>% select(ITEM)
temp4 <- full_join(temp1, temp2)
temp5 <- full_join(temp3, temp4)
cooxupe_df <- anti_join(cooxupe_df, temp5)
rm(temp1, temp2, temp3, temp4, temp5)


#########################################################
############   ABC Analysis  Preco ######################
#########################################################

######### ABC_sales ##########
# Set items' ITEM
# Set items' demands
# Set items' prices
# Construct inventory data frame

ABC <- cooxupe_df %>% select (ORG,ITEM,SO_last12, Preco, NOVO_CUSTO)
ABC <-ABC %>% rename(demand = SO_last12, price=Preco)                 #Renaming variable

# Get items' sales
ABC <- ABC %>% mutate(sales=demand*price)

# Sort sales column in descending order (by store)
ABC <- ABC[order(ABC$ORG,-ABC$sales),]


# Get the total sales
ABC <- ABC %>%  group_by(ORG) %>% mutate (Total=sum(sales, na.rm=TRUE))

# Create an empty vector for sales percentages
# Get the sales percentage for each item
ABC$percentage = ABC$sales/ABC$Total

# Import dplyr package
# Add the percentage column to inventory data frame
# Get the cumulative sum of the products sales' percentages
# Add the cumulative column to inventory data frame

ABC <- ABC %>%  group_by(ORG) %>% mutate (cumulative=cumsum(percentage))

# Add the cumulative column to inventory data frame

# Create an empty vector for items classification
# Classify the items by A, B and C
ABC$ABC_sales  = ifelse(ABC$cumulative <= .70,"A",
                  ifelse(ABC$cumulative <= .90, "B","C"))

######### ABC_Price & Cost ##########

#only 30 items (out of 7500) between 100 and 10 BRL
# considered only <10 BRL

# Classify the items by Price
# P>= 100 = A
# P < 100 = B
# P < 10  = C 
# P == 0 = ZERO

ABC$ABC_price = ifelse(ABC$price < 10,"C",
                        ifelse(ABC$price < 100, "B","A"))

ABC$ABC_cost = ifelse(ABC$NOVO_CUSTO == 0, "ZERO",
                      ifelse(ABC$NOVO_CUSTO < 100,"menos $100",
                             ifelse(ABC$NOVO_CUSTO < 1000,"entre $100-$1.000","mais $1.000")))


temp <- cooxupe_df
temp1 <- ABC %>% select (ORG, ITEM, ABC_sales, ABC_price, ABC_cost)
cooxupe_df <- full_join(temp, temp1)
rm(temp, temp1)                                #delete data frame
rm(ABC)                                        #delete data frame


####counting to check####
#cooxupe_df %>% group_by(ORG) %>% tally (SO_last30==0)
#cooxupe_df %>% group_by(ORG) %>% tally (sold_last30=="YES")
#cooxupe_df %>% tally (ABC_sales=="C")
#cooxupe_df %>% filter (ORG=="L07") %>% tally (ABC_sales=="C")
#cooxupe_df %>% filter (ORG=="L07") %>% tally (Preco<=10)
#cooxupe_df %>% filter (ORG=="L07") %>% tally (CUSTO_ITEM==0 & sold_last30=="YES")
#cooxupe_df %>% tally (CUSTO_ITEM==0 & sold_last30=="NO")
#cooxupe_df %>% tally (CUSTO_ITEM==0)

#cooxupe_df %>% filter (ORG=="L07") %>% tally (NOVO_CUSTO==0)
#cooxupe_df %>% filter (ORG=="L07") %>% tally (NOVO_CUSTO==0)

#cooxupe_df %>% tally (sold_last30=="NO")

#cooxupe_df %>% filter(ORG=="L07", Categorias == "Projeto Mínimo e Máximo") %>% tally (sold_last30=="YES")
#cooxupe_df %>% filter(ORG=="L07", Categorias == "Projeto Mínimo e Máximo") %>% tally ()


###################################################
####### ORDER VARIABILITY INTERMITTENCE    ########
###################################################

#######   Intermittent and Order Variability######
temp <-cooxupe_df %>% select(ORG, ITEM, Jan2018, Feb2018, Mar2018, Apr2018, May2018, Jun2018, Jul2018, Aug2018, Sep2018, Oct2018, Nov2018, Dec2018,
                   Jan2019, Feb2019, Mar2019, Apr2019, May2019, Jun2019, Jul2019, Aug2019, Sep2019, Oct2019, Nov2019, Dec2019,
                   Jan2020, Feb2020, Mar2020, Apr2020, May2020, Jun2020)
temp1 <- temp
temp2 <- temp
temp3 <- temp %>% select(ORG, ITEM, Jul2019, Aug2019, Sep2019, Oct2019, Nov2019, Dec2019, Jan2020, Feb2020, Mar2020, Apr2020, May2020, Jun2020)

#standard deviation on sales
temp$sd_30 <- 0
temp$sd_12 <- 0
temp$sd_norm_30 <- 0
temp$sd_norm_12 <- 0

a <- 1
while (a <= 155015) {
sdtest <- as.vector(temp[a,])
temp[a,33] <-sd(sdtest[3:32])
a= a+1
}

c <- 1
while (c <= 155015) {
  sdtest <- as.vector(temp[c,])
  temp[c,34] <-sd(sdtest[21:32])
  c= c+1
}

#normalizing the standard deviation
temp$sd_norm_30 <- rescale(temp$sd_30)
temp$sd_norm_12 <- rescale(temp$sd_12)

#average
temp$mean_sold30 <-rowMeans(temp[3:32])
temp$mean_sold12 <-rowMeans(temp[21:32])
temp$mean_sold12 <- replace(temp$mean_sold12, temp$mean_sold12<0,0) #zero for negative averages (when RMA)
#temp$sd_norm_30 <- temp$sd_30/temp$mean_sold30 #forma errada de normalizar   
#temp$sd_norm_12 <- temp$sd_12/temp$mean_sold12 #forma errada de normalizar

#quantity of zeros and intermittence
temp1$qty_zero_30<-rowSums(temp1[3:32]==0)
temp1$qty_zero_12<-rowSums(temp1[21:32]==0)
temp1$intermittence_30=temp1$qty_zero_30/30*100  #intermittent = (qty of zeros/qty of months) * 100
temp1$intermittence_12=temp1$qty_zero_12/12*100  #intermittent = (qty of zeros/qty of months) * 100

#no. of GAPS
temp2$GAP30 <- 9999
b <- 1
while (b <= 155015) {
  vtest <- as.vector(temp2[b,])
  vtest
  r <- rle(vtest)
  temp2[b,33] <-sum(r$values==0)
  b=b+1
}

temp3$GAP12 <- 9999

d <- 1
while (d <= 155015) {
  vtest <- as.vector(temp3[d,])
  vtest
  r <- rle(vtest)
  temp3[d,15] <-sum(r$values==0)
  d=d+1
}

temp4 <- full_join(temp, temp1)
temp5 <- full_join(temp2, temp3)
temp6 <- full_join(temp4, temp5)

cooxupe_df <- full_join(cooxupe_df, temp6)
rm(temp, temp1, temp2, temp3, temp4, temp5, temp6)
rm(r,sdtest,vtest,a,b,c,d)
mem_used()

#Order Variability = Std Deviation/ Average order Gap
#Average Order GAP = no. zeros/no. GAPS)
cooxupe_df$av_order_GAP30 <- cooxupe_df$qty_zero_30/cooxupe_df$GAP30
cooxupe_df$av_order_GAP30[is.nan(cooxupe_df$av_order_GAP30)] <- 0 #replace NAN by  0, when divided by 0 (no GAPS)

cooxupe_df$order_variability30 =  cooxupe_df$sd_norm_30/cooxupe_df$av_order_GAP30
cooxupe_df$order_variability30[is.nan(cooxupe_df$order_variability30)] <- 0 #replace NAN by  0, when divided by 0 (no Av_order_GAPS)

cooxupe_df$av_order_GAP12 <- cooxupe_df$qty_zero_12/cooxupe_df$GAP12
cooxupe_df$av_order_GAP12[is.nan(cooxupe_df$av_order_GAP12)] <- 0 #replace NAN by  0, when divided by 0 (no GAPS)

cooxupe_df$order_variability12 =  cooxupe_df$sd_norm_12/cooxupe_df$av_order_GAP12
cooxupe_df$order_variability12[is.nan(cooxupe_df$order_variability12)] <- 0 #replace NAN by  0, when divided by 0 (no Av_order_GAPS)

cooxupe_df <- cooxupe_df %>% relocate(av_order_GAP12, .after = av_order_GAP30)     #Relocate
cooxupe_df <- cooxupe_df %>% relocate(GAP30, GAP12, .after = qty_zero_12)     #Relocate
cooxupe_df <- cooxupe_df %>% relocate(qty_zero_30, qty_zero_12, GAP30, GAP12, .after = ABC_cost)     #Relocate

#### new STATS sample to BI   #####

#stat <- cooxupe_df %>% filter(ORG=="L07", sold_last30=="YES")
#plot(stat$intermittence_30,stat$sd_norm_30)
#test <- cooxupe_df %>% filter(ORG=="L07", sold_last12=="YES")
#plot(test$intermittence_12,test$sd_norm_12)
#rm(stat)


################################
####       SHORTAGE      #######
################################

test<- cooxupe_df %>% select(ORG, ITEM, inv_description,Jan2018:Jun2020)
test1<- cooxupe_df %>% select(ORG, ITEM, inv_description,Dec2017_stk:May2020_stk)

long <- gather(test, date, qty_sold, Jan2018:Jun2020, factor_key=TRUE)
long1 <- gather(test1, date, qty_onhand, Dec2017_stk:May2020_stk, factor_key=TRUE)
long1$date <- sapply(long1$date,gsub,pattern="2020_stk",replacement="2020")
long1$date <- sapply(long1$date,gsub,pattern="2019_stk",replacement="2019")
long1$date <- sapply(long1$date,gsub,pattern="2018_stk",replacement="2018")
long1$date <- sapply(long1$date,gsub,pattern="2017_stk",replacement="2017")
long1$date <- as.factor(long1$date)
cooxupe_df_long <- full_join(long, long1,by = c("ORG", "ITEM", "inv_description", "date"))
rm (test, test1, long, long1)

sapply(cooxupe_df_long, class)
mem_used()


#delete months of Dec2017 (no sales extraction) and June2018 (no transac extraction)
cooxupe_df_long <-cooxupe_df_long %>% filter (!date %in% c("Dec2017", "Jun2020"))

#arrange rows order to lag one row only on column
cooxupe_df_long$date <- sapply(cooxupe_df_long$date,gsub,pattern="Jan",replacement="01-31-")
cooxupe_df_long$date <- sapply(cooxupe_df_long$date,gsub,pattern="Feb",replacement="02-28-")
cooxupe_df_long$date <- sapply(cooxupe_df_long$date,gsub,pattern="Mar",replacement="03-31-")
cooxupe_df_long$date <- sapply(cooxupe_df_long$date,gsub,pattern="Apr",replacement="04-30-")
cooxupe_df_long$date <- sapply(cooxupe_df_long$date,gsub,pattern="May",replacement="05-31-")
cooxupe_df_long$date <- sapply(cooxupe_df_long$date,gsub,pattern="Jun",replacement="06-30-")
cooxupe_df_long$date <- sapply(cooxupe_df_long$date,gsub,pattern="Jul",replacement="07-31-")
cooxupe_df_long$date <- sapply(cooxupe_df_long$date,gsub,pattern="Aug",replacement="08-31-")
cooxupe_df_long$date <- sapply(cooxupe_df_long$date,gsub,pattern="Sep",replacement="09-30-")
cooxupe_df_long$date <- sapply(cooxupe_df_long$date,gsub,pattern="Oct",replacement="10-31-")
cooxupe_df_long$date <- sapply(cooxupe_df_long$date,gsub,pattern="Nov",replacement="11-30-")
cooxupe_df_long$date <- sapply(cooxupe_df_long$date,gsub,pattern="Dec",replacement="12-31-")
cooxupe_df_long$date <- as.Date(cooxupe_df_long$date, "%m-%d-%Y")                           #converting character to date
#cooxupe_df_long <- select(cooxupe_df_long, -7,-8)                                           #not used - delete column 33
sapply(cooxupe_df_long,class)

#LAG
#cooxupe_df_long <- cooxupe_df_long %>% mutate(shortage = 1:4888704)
#cooxupe_df_long <- cooxupe_df_long %>% mutate(test = lag(shortage))
#cooxupe_df_long <- select(cooxupe_df_long,-7, -8)                                           #delete columns

cooxupe_df_long <- cooxupe_df_long %>% mutate(qty_onhand_lag = lag(qty_onhand))
cooxupe_df_long[is.na(cooxupe_df_long)] = 0
cooxupe_df_long <- cooxupe_df_long %>% mutate(shortage = ifelse(qty_sold<=0 & qty_onhand==0 & qty_onhand_lag==0,"YES","NO"))

shortage_wide <- cooxupe_df_long 
shortage_wide <- select(shortage_wide,-5, -6,-7)                                           #delete columns
shortage_wide <- dcast(cooxupe_df_long, ORG + ITEM + inv_description ~ date, value.var="shortage")
shortage_wide <- shortage_wide %>% select(ORG,ITEM,"2019-07-31","2019-08-31","2019-09-30","2019-10-31",
                                          "2019-11-30","2019-12-31","2020-01-31","2020-01-31","2020-02-28","2020-03-31","2020-04-30","2020-05-31")
shortage_wide <- shortage_wide %>% rename(short_Jul19 = "2019-07-31")
shortage_wide <- shortage_wide %>% rename(short_Aug19 = "2019-08-31")
shortage_wide <- shortage_wide %>% rename(short_Sep19 = "2019-09-30")
shortage_wide <- shortage_wide %>% rename(short_Oct19 = "2019-10-31")
shortage_wide <- shortage_wide %>% rename(short_Nov19 = "2019-11-30")
shortage_wide <- shortage_wide %>% rename(short_Dec19 = "2019-12-31")
shortage_wide <- shortage_wide %>% rename(short_Jan20 = "2020-01-31")
shortage_wide <- shortage_wide %>% rename(short_Feb20 = "2020-02-28")
shortage_wide <- shortage_wide %>% rename(short_Mar20 = "2020-03-31")
shortage_wide <- shortage_wide %>% rename(short_Apr20 = "2020-04-30")
shortage_wide <- shortage_wide %>% rename(short_May20 = "2020-05-31")

#including shortage per month on cooxupe_dataframe 
cooxupe_df <- left_join(cooxupe_df, shortage_wide)

rm(shortage_wide)
 

################################
####       MIN-MAX      ########
################################
# general list of items & description for the project
general_itemlist <- cooxupe_df %>% select (ITEM, inv_description, Categorias) %>% distinct(ITEM, .keep_all = TRUE)

#Selecting variables for the table MIN-MAX
min_max <- cooxupe_df %>%  select(ORG, ITEM, inv_description, Jan2018, Feb2018, Mar2018, Apr2018, 
                                  May2018, Jun2018, Jul2018, Aug2018, Sep2018, Oct2018, Nov2018, Dec2018,
                                  Jan2019, Feb2019, Mar2019, Apr2019, May2019, Jun2019, Jul2019, Aug2019,
                                  Sep2019, Oct2019, Nov2019, Dec2019,Jan2020, Feb2020, Mar2020, Apr2020, 
                                  May2020, Jun2020,SO_last30, SO_last12, mean_sold30, mean_sold12, intermittence_30, intermittence_12, 
                                  sd_30, sd_12,`Controle Expiração`, Categorias)



#Selecting only the sales columns for calculate the mean (considering only months with positive sales)
temp <- min_max %>%  select(ORG, ITEM, Jan2018, Feb2018, Mar2018, Apr2018, May2018, Jun2018, Jul2018, Aug2018, Sep2018, Oct2018, Nov2018, Dec2018,
         Jan2019, Feb2019, Mar2019, Apr2019, May2019, Jun2019, Jul2019, Aug2019, Sep2019, Oct2019, Nov2019, Dec2019,
         Jan2020, Feb2020, Mar2020, Apr2020, May2020, Jun2020)

#mean considering only months with positive sales
temp[temp == 0] <- NA
temp$mean_sold30_diff0 <-rowMeans(temp[3:32], na.rm = "TRUE")
temp$mean_sold12_diff0 <-rowMeans(temp[21:32], na.rm = "TRUE")
temp[is.na(temp)] <- 0
temp$mean_sold30_diff0 <- replace(temp$mean_sold30_diff0, temp$mean_sold30_diff0<0,0) #zero for negative averages (when RMA)
temp$mean_sold12_diff0 <- replace(temp$mean_sold12_diff0, temp$mean_sold12_diff0<0,0) #zero for negative averages (when RMA)
temp <- temp %>% select(ORG, ITEM,mean_sold30_diff0, mean_sold12_diff0)

#join min-max and temp
min_max <- left_join(min_max, temp)
min_max <- min_max %>% relocate(mean_sold30_diff0, mean_sold12_diff0, .after = mean_sold12)
rm(temp)

#MIN-MAX - selecting full mean of months with positive sales / rounding THE VALUE
#Low intermittent (<30%) 1 month for regular items, and .7 month (~20 days)  for items with expiration date
min_max$`Controle Expiração`[is.na(min_max$`Controle Expiração`)] <- "Não"

min_max <- min_max %>% mutate(MIN_12 = ifelse(intermittence_12<=30 & `Controle Expiração`=="Sim",round(mean_sold12*.7),
                                              ifelse(intermittence_12<=30 & `Controle Expiração`=="Não",round(mean_sold12),
                                                     ifelse(intermittence_12>30 & intermittence_12<90 & `Controle Expiração`=="Sim", round(mean_sold12_diff0*.7),
                                                            ifelse(intermittence_12>30 & intermittence_12<90 & `Controle Expiração`=="Não",round(mean_sold12_diff0),
                                                                   ifelse(intermittence_12>=90 & intermittence_12<100,round(mean_sold12_diff0*.5001), 0))))))

min_max <- min_max %>% mutate(MAX_12 = 2* MIN_12)
min_max <- min_max %>% mutate(MIN_30 = ifelse(intermittence_30<=30 & `Controle Expiração`=="Sim",round(mean_sold30*.7),
                                              ifelse(intermittence_30<=30 & `Controle Expiração`=="Não",round(mean_sold30),
                                                     ifelse(intermittence_30>30 & intermittence_30<90 & `Controle Expiração`=="Sim", round(mean_sold30_diff0*.7),
                                                            ifelse(intermittence_30>30 & intermittence_30<90 & `Controle Expiração`=="Não",round(mean_sold30_diff0),
                                                                   ifelse(intermittence_30>=90 & intermittence_30<100,round(mean_sold30_diff0*.5001), 0))))))

min_max <- min_max %>% mutate(MAX_30 = 2* MIN_30)
min_max <- min_max %>% relocate(MIN_30, MAX_30, MIN_12, MAX_12, .after = sd_12)
min_max <- min_max %>% select (-34, -36, -38, -40,-42, -44, -45)
min_max_0 <- min_max %>% filter(ORG !="J48", ORG !="F44", ORG != "F45", Categorias=="Projeto Mínimo e Máximo")

############################################################
##### MIN-MAX for Items w NO Historical - 1st criteria  ####
############################################################

#finding a MIN-MAX based on average of the items sold

#remove all items with mean_sold ==0, then calculate the average by item considering all ORG's (stores / UAs) 
min_max_hist  <- min_max_0 %>% filter(mean_sold12 >0)  #ordered by ITEM & qty sold
min_max_hist <- min_max_hist %>% arrange(ITEM, desc(SO_last12))  #ordered by ITEM & qty sold

#selecting different data-frames for stores and UAs, # setting 1 when mean < 0.5 because MIN_12 rounded to zero
min_max_st <- min_max_hist %>% filter(ORG %in% c("L03","L04","L07","L09","L10","L11","L12","L14","L15","L18", "L19","L01","L05","L06","L08","L16","L20","076")) %>% select(ORG, ITEM, inv_description,SO_last12, mean_sold12, mean_sold12_diff0, intermittence_12, MIN_12, MAX_12, `Controle Expiração`, Categorias)
min_max_st <- min_max_st %>% mutate(MIN_12 = case_when(MIN_12 == 0 ~ 1, TRUE   ~ MIN_12))
min_max_st <- min_max_st %>% mutate(MAX_12 = 2* MIN_12)
min_max_UA <- min_max_hist %>% filter(ORG %in% c("300","301","302","303","306","307","308","309","310","311", "312","314")) %>% select(ORG, ITEM, inv_description,SO_last12, mean_sold12, mean_sold12_diff0, intermittence_12, MIN_12, MAX_12, `Controle Expiração`, Categorias)
min_max_UA <- min_max_UA %>% mutate(MIN_12 = case_when(MIN_12 == 0 ~ 1, TRUE   ~ MIN_12))
min_max_UA <- min_max_UA %>% mutate(MAX_12 = 2* MIN_12)


av_stores <- min_max_hist %>%  filter(ORG %in% c("L03","L04","L07","L09","L10","L11","L12","L14","L15","L18", "L19","L01","L05","L06","L08","L16","L20","076")) %>%
                                      group_by(ITEM, `Controle Expiração`, Categorias) %>% 
                                        summarise(SO_last12=mean(SO_last12), mean_sold12=mean(mean_sold12), mean_sold12_diff0=mean(mean_sold12_diff0), intermittence_12=mean(intermittence_12))

av_UAs <- min_max_hist %>%  filter(ORG %in% c("300","301","302","303","306","307","308","309","310","311", "312","314")) %>%
                              group_by(ITEM, `Controle Expiração`, Categorias) %>% 
                                summarise(SO_last12=mean(SO_last12), mean_sold12=mean(mean_sold12), mean_sold12_diff0=mean(mean_sold12_diff0), intermittence_12=mean(intermittence_12))

#calculate MIN-MAX 'average'
av_stores <- av_stores %>% mutate(MIN_12 = ifelse(intermittence_12<=30 & `Controle Expiração`=="Sim",round(mean_sold12*.7),
                                              ifelse(intermittence_12<=30 & `Controle Expiração`=="Não",round(mean_sold12),
                                                     ifelse(intermittence_12>30 & intermittence_12<90 & `Controle Expiração`=="Sim", round(mean_sold12_diff0*.7),
                                                            ifelse(intermittence_12>30 & intermittence_12<90 & `Controle Expiração`=="Não",round(mean_sold12_diff0),
                                                                   ifelse(intermittence_12>=90 & intermittence_12<100,round(mean_sold12_diff0*.5001), 0))))))

av_stores <- av_stores %>% mutate(MAX_12 = 2* MIN_12)
av_stores <- av_stores %>% relocate(`Controle Expiração`, Categorias, .after = MAX_12)

av_UAs <- av_UAs %>% mutate(MIN_12 = ifelse(intermittence_12<=30 & `Controle Expiração`=="Sim",round(mean_sold12*.7),
                                                  ifelse(intermittence_12<=30 & `Controle Expiração`=="Não",round(mean_sold12),
                                                         ifelse(intermittence_12>30 & intermittence_12<90 & `Controle Expiração`=="Sim", round(mean_sold12_diff0*.7),
                                                                ifelse(intermittence_12>30 & intermittence_12<90 & `Controle Expiração`=="Não",round(mean_sold12_diff0),
                                                                       ifelse(intermittence_12>=90 & intermittence_12<100,round(mean_sold12_diff0*.5001), 0))))))

av_UAs <- av_UAs %>% mutate(MAX_12 = 2* MIN_12)
av_UAs <- av_UAs %>% relocate(`Controle Expiração`, Categorias, .after = MAX_12)

#joining the average of sold items for stores with NO items sold
temp_nohist_st <- min_max_0 %>% filter (mean_sold12==0,ORG %in% c("L03","L04","L07","L09","L10","L11","L12","L14","L15","L18", "L19","L01","L05","L06","L08","L16","L20","076"))%>% 
  select(ORG, ITEM, inv_description)
temp_nohist_UAs <- min_max_0 %>% filter (mean_sold12==0,ORG %in% c("300","301","302","303","306","307","308","309","310","311", "312","314")) %>%
  select(ORG, ITEM, inv_description)

min_max_1st_st <- left_join(temp_nohist_st,av_stores, by = "ITEM")
min_max_1st_UA <- left_join(temp_nohist_UAs,av_UAs, by = "ITEM")
min_max_1st_st[is.na(min_max_1st_st)] <- 0
min_max_1st_UA[is.na(min_max_1st_UA)] <- 0
rm(temp_nohist_st, temp_nohist_UAs)

# data-frame to start the 2nd criteria
min_max_2nd_st <- min_max_1st_st %>% filter (SO_last12==0) %>% select(ORG, ITEM, inv_description)
min_max_2nd_UA <- min_max_1st_UA %>% filter (SO_last12==0) %>% select(ORG, ITEM, inv_description)

# cleaning the 1st criteria
min_max_1st_st <- min_max_1st_st %>% filter(SO_last12>0)
min_max_1st_UA <- min_max_1st_UA %>% filter(SO_last12>0)

# setting 1 when mean < 0.5 because MIN_12 rounded to zero
min_max_1st_st <- min_max_1st_st %>% mutate(MIN_12 = case_when(MIN_12 == 0 ~ 1, TRUE   ~ MIN_12))
min_max_1st_st <- min_max_1st_st %>% mutate(MAX_12 = 2* MIN_12)
min_max_1st_UA <- min_max_1st_UA %>% mutate(MIN_12 = case_when(MIN_12 == 0 ~ 1, TRUE   ~ MIN_12))
min_max_1st_UA <- min_max_1st_UA %>% mutate(MAX_12 = 2* MIN_12)


############################################################
##### MIN-MAX for Items w NO Historical - 2nd criteria  ####
############################################################


temp_st <- min_max %>% select(ORG, ITEM, inv_description, SO_last12, mean_sold12, mean_sold12_diff0, intermittence_12, MIN_12, MAX_12) %>%  filter(ORG %in% c("L03","L04","L07","L09","L10","L11","L12","L14","L15","L18", "L19","L01","L05","L06","L08","L16","L20","076"))
temp_st <- temp_st %>% mutate (match_description =inv_description)
temp_st <- temp_st %>% relocate(match_description, .after = ITEM)
temp_st$match_description = str_sub(temp_st$match_description,1,5)
temp_st <- temp_st %>% arrange(match_description, desc(SO_last12))  #ordered by short_descrip & qty sold
temp_st <- temp_st %>% distinct(match_description, .keep_all = TRUE)

temp_UA <- min_max %>% select(ORG, ITEM, inv_description, SO_last12, mean_sold12, mean_sold12_diff0, intermittence_12, MIN_12, MAX_12) %>%  filter(ORG %in% c("300","301","302","303","306","307","308","309","310","311", "312","314"))
temp_UA <- temp_UA %>% mutate (match_description =inv_description)
temp_UA <- temp_UA %>% relocate(match_description, .after = ITEM)
temp_UA$match_description = str_sub(temp_UA$match_description,1,5)
temp_UA <- temp_UA %>% arrange(match_description, desc(SO_last12))  #ordered by short_descrip & qty sold
temp_UA <- temp_UA %>% distinct(match_description, .keep_all = TRUE)


#get only the 5 firsts characters from 2nd criteria
min_max_2nd_st <- min_max_2nd_st %>% mutate (match_description =inv_description)
min_max_2nd_st <- min_max_2nd_st %>% relocate(match_description, .after = ITEM)
min_max_2nd_st$match_description = str_sub(min_max_2nd_st$match_description,1,5)

min_max_2nd_UA <- min_max_2nd_UA %>% mutate (match_description =inv_description)
min_max_2nd_UA <- min_max_2nd_UA %>% relocate(match_description, .after = ITEM)
min_max_2nd_UA$match_description = str_sub(min_max_2nd_UA$match_description,1,5)

min_max_2nd_st <-left_join(min_max_2nd_st,temp_st, by = "match_description")
min_max_2nd_UA <-left_join(min_max_2nd_UA,temp_UA, by = "match_description")

# data-frame to the 3rd criteria
min_max_3rd_st <- min_max_2nd_st %>% filter (SO_last12==0) %>% select(ORG.x, ITEM.x, inv_description.x,SO_last12, mean_sold12, mean_sold12_diff0, intermittence_12, MIN_12, MAX_12)
min_max_3rd_UA <- min_max_2nd_UA %>% filter (SO_last12==0) %>% select(ORG.x, ITEM.x, inv_description.x,SO_last12, mean_sold12, mean_sold12_diff0, intermittence_12, MIN_12, MAX_12)

# cleaning the 2nd criteria
min_max_2nd_st <- min_max_2nd_st %>% filter(SO_last12>0)
min_max_2nd_UA <- min_max_2nd_UA %>% filter(SO_last12>0)
rm(temp_st, temp_UA)

# setting 1 when mean < 0.5 because MIN_12 rounded to zero
min_max_2nd_st <- min_max_2nd_st %>% mutate(MIN_12 = case_when(MIN_12 == 0 ~ 1, TRUE   ~ MIN_12))
min_max_2nd_st <- min_max_2nd_st %>% mutate(MAX_12 = 2* MIN_12)
min_max_2nd_UA <- min_max_2nd_UA %>% mutate(MIN_12 = case_when(MIN_12 == 0 ~ 1, TRUE   ~ MIN_12))
min_max_2nd_UA <- min_max_2nd_UA %>% mutate(MAX_12 = 2* MIN_12)
rm(av_stores, av_UAs)
#rm(min_max_st, min_max_UA,min_max, min_max_1st_st, min_max_1st_UA, min_max_2nd_st, min_max_2nd_UA, min_max_3rd_st, min_max_3rd_UA, min_max_hist, av_stores, av_UAs, min_max_0)

#counting (L07)
min_max_0 %>% filter (Categorias=="Projeto Mínimo e Máximo") %>% tally()
min_max_st %>% filter(ORG=="L07", intermittence_12<=30) %>% tally()
min_max_st %>% filter(ORG=="L07", intermittence_12>30 & intermittence_12<74) %>% tally()
min_max_st %>% filter(ORG=="L07", intermittence_12>=74) %>% tally()
min_max_1st_st %>% filter(ORG=="L07") %>% tally()
min_max_2nd_st %>% filter(ORG.x=="L07") %>% tally()
min_max_3rd_st %>% filter(ORG.x=="L07") %>% tally()

#counting (L07)
min_max_UA %>% filter(ORG=="301", intermittence_12<=30) %>% tally()
min_max_UA %>% filter(ORG=="301", intermittence_12>30 & intermittence_12<74) %>% tally()
min_max_UA %>% filter(ORG=="301", intermittence_12>=74) %>% tally()
min_max_1st_UA %>% filter(ORG=="301") %>% tally()
min_max_2nd_UA %>% filter(ORG.x=="301") %>% tally()
min_max_3rd_UA %>% filter(ORG.x=="301") %>% tally()


###################################################
##### MIN-MAX MANUAL-UPDATE FROM THE ANALISTS #####
#####         2nd and 3rd CRITERIAS           #####
###################################################

####### 2nd criteria - 5 letters - first spreadsheet ########
#lojas
min_max_2nd_st_u <- read_excel("C:/Users/Juliano/Dropbox/Cooxupe/6_Planilhas_geradas/atualiz_manual_cooxupe/Min_max_lojas - 2º Critério por descrição.xlsx", sheet  = "Planilha1")
min_max_2nd_st_u <- min_max_2nd_st_u %>% filter(is.na(`Estoque Estratégico CDI`)) %>% rename(ITEM.x = ITEM)
min_max_2nd_st_u <- min_max_2nd_st_u %>% select(ITEM.x, `Loja MIN_12`, `Loja MAX_12`)
#test_duplicate <- min_max_2nd_st_u %>% group_by (ITEM.x) %>% filter (n()>1)

#UAS
min_max_2nd_UA_u <- read_excel("C:/Users/Juliano/Dropbox/Cooxupe/6_Planilhas_geradas/atualiz_manual_cooxupe/Min_max_UAs - 2º Critério por descrição.xlsx", sheet  = "Min e Max")
min_max_2nd_UA_u <- min_max_2nd_UA_u %>% select(ITEM.x, Mínimo, Máximo)
#test_duplicate <- min_max_2nd_UA_u %>% group_by (ITEM.x) %>% filter (n()>1)
min_max_2nd_UA_u <- min_max_2nd_UA_u %>% distinct(ITEM.x,.keep_all = TRUE) #remove 2 duplicated lines from the spreadsheet

#filtering ITEMS 'out-of-scope' and 'Estratégico CDI'
#lojas
temp <- read_excel("C:/Users/Juliano/Dropbox/Cooxupe/4_Planilhas_extraidas/Cópia de Itens Zerados UAs e Lojas.xlsx", sheet  = "Lojas Manual")
temp1 <- temp %>% filter (Obs.=="Encomenda" | Obs.=="Fora do projeto" ) %>% select(ITEM.x, MIN, MAX)
min_max_2nd_st_u <- anti_join(min_max_2nd_st_u, temp1, by="ITEM.x")

#UAs
temp2 <- read_excel("C:/Users/Juliano/Dropbox/Cooxupe/4_Planilhas_extraidas/Cópia de Itens Zerados UAs e Lojas.xlsx", sheet  = "UAs Manual")
temp2 <- temp2 %>% distinct(ITEM.x,.keep_all = TRUE) #remove 2 duplicated lines from the spreadsheet
temp3 <- temp2 %>% filter (Obs.=="Encomenda" | Obs.=="Fora do projeto" ) %>% select(ITEM.x, MIN, MAX, Obs.)
min_max_2nd_UA_u <- anti_join(min_max_2nd_UA_u, temp3, by="ITEM.x")
#test_duplicate <- min_max_2nd_UA_u %>% group_by (ITEM.x) %>% filter (n()>1)



##manual INPUT - second spreadsheet
#lojas
temp4 <- temp %>% filter (is.na(Obs.)) %>% select(ITEM.x, MIN, MAX)
min_max_2nd_st_u <- left_join(min_max_2nd_st_u, temp4, by="ITEM.x")
min_max_2nd_st_u <- min_max_2nd_st_u %>% mutate(`Loja MIN_12` = ifelse(`Loja MIN_12`==0, MIN, `Loja MIN_12`))
min_max_2nd_st_u <- min_max_2nd_st_u %>% mutate(`Loja MAX_12` = ifelse(`Loja MAX_12`==0, MAX, `Loja MAX_12`))
min_max_2nd_st_u <- min_max_2nd_st_u %>% select (-4,-5)
#UAS
temp5 <- temp2 %>% filter (is.na(Obs.)| Obs.=="Sem Estoque Uas") %>% select(ITEM.x, MIN, MAX, Obs.)
min_max_2nd_UA_u <- left_join(min_max_2nd_UA_u, temp5,by="ITEM.x")
min_max_2nd_UA_u <- min_max_2nd_UA_u %>% mutate(Mínimo = ifelse(Mínimo==0, MIN, Mínimo))
min_max_2nd_UA_u <- min_max_2nd_UA_u %>% mutate(Máximo = ifelse(Máximo==0, MAX, Máximo))
min_max_2nd_UA_u <- min_max_2nd_UA_u %>% select (-4,-5)


#generating final spreadsheet 2nd criteria
min_max_2nd_st <- left_join(min_max_2nd_st, min_max_2nd_st_u, by="ITEM.x")
min_max_2nd_st <- min_max_2nd_st %>% select (ORG.x, ITEM.x, inv_description.x, `Loja MIN_12`, `Loja MAX_12`)
min_max_2nd_st <- min_max_2nd_st %>% rename (MIN_12 = `Loja MIN_12`, MAX_12=`Loja MAX_12`)

min_max_2nd_UA <- left_join(min_max_2nd_UA, min_max_2nd_UA_u, by="ITEM.x")
min_max_2nd_UA <- min_max_2nd_UA %>% select (ORG.x, ITEM.x, inv_description.x, Mínimo, Máximo, Obs.)
min_max_2nd_UA <- min_max_2nd_UA %>% rename (MIN_12 = Mínimo, MAX_12=Máximo)

   
####### 3rd criteria - manual with zeros ################
#Lojas
min_max_3rd_st_u <- read_excel("C:/Users/Juliano/Dropbox/Cooxupe/6_Planilhas_geradas/atualiz_manual_cooxupe/Min_max_lojas - Manual - Sem Histórico.xlsx", sheet  = "Manual Lojas")
min_max_3rd_st_u <- min_max_3rd_st_u %>% filter(is.na(Obs.) | Obs.!="estoque estratégico CDI")
min_max_3rd_st_u <- min_max_3rd_st_u %>% select(ITEM.x, MIN, MAX, Obs.)
#test_duplicate <- min_max_3rd_st_u %>% group_by (ITEM.x) %>% filter (n()>1)

#UAS
min_max_3rd_UA_u <- read_excel("C:/Users/Juliano/Dropbox/Cooxupe/6_Planilhas_geradas/atualiz_manual_cooxupe/Min_max_UAs - Manual - Sem Histórico.xlsx", sheet  = "Planilha1")
min_max_3rd_UA_u <- min_max_3rd_UA_u %>% filter(is.na(Obs.) | Obs.!="estoque estratégico CDI")
min_max_3rd_UA_u <- min_max_3rd_UA_u %>% select(ITEM.x, MIN, MAX, Obs.)
test_duplicate <- min_max_3rd_UA_u %>% group_by (ITEM.x) %>% filter (n()>1)
min_max_3rd_UA_u <- min_max_3rd_UA_u %>% distinct(ITEM.x,.keep_all = TRUE) #remove 70 duplicated lines from the spreadsheet

#filtering ITEMS 'out-of-scope' and 'Estratégico CDI'
#lojas
min_max_3rd_st_u <- anti_join(min_max_3rd_st_u, temp1, by="ITEM.x")
#UAs
min_max_3rd_UA_u <- anti_join(min_max_3rd_UA_u, temp3, by="ITEM.x")

##manual INPUT - second spreadsheet
#lojas
min_max_3rd_st_u <- left_join(min_max_3rd_st_u, temp4, by="ITEM.x")
min_max_3rd_st_u <- min_max_3rd_st_u %>% mutate(MIN.x = ifelse(MIN.x==0, MIN.y, MIN.x))
min_max_3rd_st_u <- min_max_3rd_st_u %>% mutate(MAX.x = ifelse(MAX.x==0, MAX.y, MAX.x))
min_max_3rd_st_u <- min_max_3rd_st_u %>% select (-4,-5,-6)
min_max_3rd_st_u <- min_max_3rd_st_u %>% rename (MIN=MIN.x, MAX=MAX.x)

#UAS
test_duplicate <- temp5 %>% group_by (ITEM.x) %>% filter (n()>1)
min_max_3rd_UA_u <- left_join(min_max_3rd_UA_u, temp5, by="ITEM.x")
min_max_3rd_UA_u <- min_max_3rd_UA_u %>% mutate(MIN.x = ifelse(MIN.x==0, MIN.y, MIN.x))
min_max_3rd_UA_u <- min_max_3rd_UA_u %>% mutate(MAX.x = ifelse(MAX.x==0, MAX.y, MAX.x))
min_max_3rd_UA_u <- min_max_3rd_UA_u %>% mutate(MIN.x = ifelse(ITEM.x==34624, MIN.y, MIN.x))
min_max_3rd_UA_u <- min_max_3rd_UA_u %>% mutate(MAX.x = ifelse(ITEM.x==34624, MAX.y, MAX.x))
min_max_3rd_UA_u <- min_max_3rd_UA_u %>% mutate(MIN.x = ifelse(ITEM.x==34621, MIN.y, MIN.x))
min_max_3rd_UA_u <- min_max_3rd_UA_u %>% mutate(MAX.x = ifelse(ITEM.x==34621, MAX.y, MAX.x))
min_max_3rd_UA_u <- min_max_3rd_UA_u %>% select (-4,-5,-6)
min_max_3rd_UA_u <- min_max_3rd_UA_u %>% rename (MIN=MIN.x, MAX=MAX.x, Obs.=Obs..y)

#generating final spreadsheet 3rd criteria
min_max_3rd_st <- left_join(min_max_3rd_st, min_max_3rd_st_u, by="ITEM.x")
min_max_3rd_st <- min_max_3rd_st %>% select (ORG.x, ITEM.x, inv_description.x, MIN, MAX)
min_max_3rd_st <- min_max_3rd_st %>% rename (MIN_12 = MIN, MAX_12=MAX)

min_max_3rd_UA <- left_join(min_max_3rd_UA, min_max_3rd_UA_u, by="ITEM.x")
min_max_3rd_UA <- min_max_3rd_UA %>% select (ORG.x, ITEM.x, inv_description.x, MIN, MAX, Obs.)
min_max_3rd_UA <- min_max_3rd_UA %>% rename (MIN_12 = MIN, MAX_12=MAX)

rm(temp,temp1, temp2, temp3, temp4, temp5)
rm(min_max_2nd_st_u, min_max_2nd_UA_u, min_max_3rd_st_u, min_max_3rd_UA_u, test_duplicate)



#########################################
#####    MIN-MAX E.Seg.  CDI      #######
#########################################

#selecting the MIN for all ORGs and ITEMS
temp  <- min_max_st %>% select (ORG, ITEM, MIN_12)
temp1 <- min_max_UA %>% select (ORG, ITEM, MIN_12)
temp2 <- min_max_1st_st  %>% select (ORG, ITEM, MIN_12)
temp3 <- min_max_1st_UA  %>% select (ORG, ITEM, MIN_12)
temp4 <- min_max_2nd_st  %>% select (ORG.x, ITEM.x, MIN_12) %>% rename (ORG=ORG.x, ITEM = ITEM.x)
temp5 <- min_max_2nd_UA  %>% select (ORG.x, ITEM.x, MIN_12) %>% rename (ORG=ORG.x, ITEM = ITEM.x)
temp6 <- min_max_3rd_st  %>% select (ORG.x, ITEM.x, MIN_12) %>% rename (ORG=ORG.x, ITEM = ITEM.x)
temp7 <- min_max_3rd_UA  %>% select (ORG.x, ITEM.x, MIN_12) %>% rename (ORG=ORG.x, ITEM = ITEM.x)
temp8  <- full_join(temp1, temp)
temp9  <- full_join(temp8, temp2)
temp10 <- full_join(temp9, temp3)
temp11 <- full_join(temp10, temp4)
temp12 <- full_join(temp11, temp5)
temp13 <- full_join(temp12, temp6)
temp14 <- full_join(temp13, temp7)
rm(temp, temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8, temp9, temp10, temp11, temp12, temp13)

#selecting the MIN for all ORG and ITEMS
min_max_CDI_J48  <- temp14 %>% filter (ORG %in% c("J48","F44","300","301","302","303","306","307","308","309","310","311", "312","314","L01","L03","L04","L06","L07","L08","L09","L10","L12","L14", "L15", "L16", "L18","L19", "L20","076")) %>% group_by(ITEM) %>% summarise(Eseg_sumMIN=sum(MIN_12))
LT_J48 <- cooxupe_df %>% filter (ORG=="J48") %>% select (ITEM, inv_description, PREPROCESSING_LEAD_TIME, PROCESSING_LEAD_TIME, POSTPROCESSING_LEAD_TIME)
min_max_CDI_J48 <- left_join(min_max_CDI_J48, LT_J48)
min_max_CDI_J48 <- min_max_CDI_J48 %>% relocate(inv_description, .after=ITEM)
min_max_CDI_J48 <- min_max_CDI_J48 %>% relocate(Eseg_sumMIN, .after=POSTPROCESSING_LEAD_TIME)

min_max_CDI_F45  <- temp14 %>% filter (ORG %in% c("L05","L11")) %>% group_by(ITEM) %>% summarise(Eseg_sumMIN=sum(MIN_12))
LT_F45 <- cooxupe_df %>% filter (ORG=="F45") %>% select (ITEM, inv_description, PREPROCESSING_LEAD_TIME, PROCESSING_LEAD_TIME, POSTPROCESSING_LEAD_TIME)
min_max_CDI_F45 <- left_join(min_max_CDI_F45, LT_F45)
min_max_CDI_F45 <- min_max_CDI_F45 %>% relocate(inv_description, .after=ITEM)
min_max_CDI_F45 <- min_max_CDI_F45 %>% relocate(Eseg_sumMIN, .after=POSTPROCESSING_LEAD_TIME)

rm(temp14)

#Eseg_LT
min_max_CDI_J48 <- min_max_CDI_J48 %>% mutate(ESeg_LT = round(Eseg_sumMIN/21*(PREPROCESSING_LEAD_TIME+PROCESSING_LEAD_TIME)))
min_max_CDI_F45 <- min_max_CDI_F45 %>% mutate(ESeg_LT = round(Eseg_sumMIN/21*(PREPROCESSING_LEAD_TIME+PROCESSING_LEAD_TIME)))

#Eseg
min_max_CDI_J48 <- min_max_CDI_J48 %>% mutate(ESeg = Eseg_sumMIN+ESeg_LT)
min_max_CDI_F45 <- min_max_CDI_F45 %>% mutate(ESeg = Eseg_sumMIN+ESeg_LT)

#calculating the intermittence, mean and sold_last12#
#J48
min_max_CDI_J48_0 <- min_max_0 %>% filter(ORG %in% c("J48","F44","300","301","302","303","306","307","308","309","310","311", "312","314","L01","L03","L04","L06","L07","L08","L09","L10","L12","L14", "L15", "L16", "L18","L19", "L20","076")) %>% 
  select (ORG, ITEM, inv_description, Jul2019, Aug2019, Sep2019, Oct2019, Nov2019, Dec2019,Jan2020, Feb2020, Mar2020, Apr2020, May2020, Jun2020)

min_max_CDI_J48_0 <- min_max_CDI_J48_0 %>% group_by(ITEM) %>% summarise(Jul2019=sum(Jul2019),Aug2019=sum(Aug2019),Sep2019=sum(Sep2019),Oct2019=sum(Oct2019),Nov2019=sum(Nov2019),Dec2019=sum(Dec2019),
                                                                     Jan2020=sum(Jan2020),Feb2020=sum(Feb2020),Mar2020=sum(Mar2020),Apr2020=sum(Apr2020),May2020=sum(May2020),Jun2020=sum(Jun2020))
min_max_CDI_J48_0$SO_last12 <-rowSums(min_max_CDI_J48_0[2:13])
min_max_CDI_J48_0$mean_sold12 <-rowMeans(min_max_CDI_J48_0[2:13])

#F45
min_max_CDI_F45_0 <- min_max_0 %>% filter(ORG %in% c("L05","L11")) %>% 
  select (ORG, ITEM, inv_description, Jul2019, Aug2019, Sep2019, Oct2019, Nov2019, Dec2019,Jan2020, Feb2020, Mar2020, Apr2020, May2020, Jun2020)

min_max_CDI_F45_0 <- min_max_CDI_F45_0 %>% group_by(ITEM) %>% summarise(Jul2019=sum(Jul2019),Aug2019=sum(Aug2019),Sep2019=sum(Sep2019),Oct2019=sum(Oct2019),Nov2019=sum(Nov2019),Dec2019=sum(Dec2019),
                                                                        Jan2020=sum(Jan2020),Feb2020=sum(Feb2020),Mar2020=sum(Mar2020),Apr2020=sum(Apr2020),May2020=sum(May2020),Jun2020=sum(Jun2020))
min_max_CDI_F45_0$SO_last12 <-rowSums(min_max_CDI_F45_0[2:13])
min_max_CDI_F45_0$mean_sold12 <-rowMeans(min_max_CDI_F45_0[2:13])

#mean considering only months with positive sales
min_max_CDI_J48_0[min_max_CDI_J48_0 == 0] <- NA
min_max_CDI_J48_0$mean_sold12_diff0 <-rowMeans(min_max_CDI_J48_0[2:13], na.rm = "TRUE")
min_max_CDI_J48_0[is.na(min_max_CDI_J48_0)] <- 0
min_max_CDI_F45_0[min_max_CDI_F45_0 == 0] <- NA
min_max_CDI_F45_0$mean_sold12_diff0 <-rowMeans(min_max_CDI_F45_0[2:13], na.rm = "TRUE")
min_max_CDI_F45_0[is.na(min_max_CDI_F45_0)] <- 0

#quantity of zeros and intermittence
min_max_CDI_J48_0$qty_zero_12<-rowSums(min_max_CDI_J48_0[2:13]==0)
min_max_CDI_J48_0$intermittence_12=min_max_CDI_J48_0$qty_zero_12/12*100  #intermittent = (qty of zeros/qty of months) * 100
min_max_CDI_J48_0 <- min_max_CDI_J48_0 %>% select(-17)

min_max_CDI_F45_0$qty_zero_12<-rowSums(min_max_CDI_F45_0[4:15]==0)
min_max_CDI_F45_0$intermittence_12=min_max_CDI_F45_0$qty_zero_12/12*100  #intermittent = (qty of zeros/qty of months) * 100
min_max_CDI_F45_0 <- min_max_CDI_F45_0 %>% select(-17)

#join and relocate
min_max_CDI_J48 <- left_join(min_max_CDI_J48, min_max_CDI_J48_0)
min_max_CDI_F45 <- left_join(min_max_CDI_F45, min_max_CDI_F45_0)
rm(min_max_CDI_J48_0, min_max_CDI_F45_0)
min_max_CDI_J48 <- min_max_CDI_J48 %>% relocate (Eseg_sumMIN, ESeg_LT, ESeg, .after=intermittence_12)
min_max_CDI_F45 <-min_max_CDI_F45 %>% relocate (Eseg_sumMIN, ESeg_LT, ESeg, .after=intermittence_12)

min_max_CDI_J48 <- min_max_CDI_J48 %>% select(ITEM, Eseg_sumMIN)
min_max_CDI_F45 <- min_max_CDI_F45 %>% select(ITEM, Eseg_sumMIN)
min_max_CDI_J48 <- left_join(min_max_CDI_J48, general_itemlist)
min_max_CDI_F45 <- left_join(min_max_CDI_F45, general_itemlist)
min_max_CDI_J48 <- min_max_CDI_J48 %>% relocate (inv_description, .after=ITEM)
min_max_CDI_F45 <-min_max_CDI_F45 %>% relocate (inv_description, .after=ITEM)

#########################################
##### STRATEGICAL ITEMS IN CDI     ######
#########################################

#CDI J48 + F44
strat_CDI_J48 <- min_max %>% filter(ORG %in% c("J48","F44","300","301","302","303","306","307","308","309","310","311", "312","314","L01","L03","L04","L06","L07","L08","L09","L10","L12","L14", "L15", "L16", "L18","L19", "L20","076"),
                                    Categorias=="Estoque estratégico apenas no CDI") %>% 
                          select (ORG, ITEM, inv_description, Jul2019, Aug2019, Sep2019, Oct2019, Nov2019, Dec2019,
                            Jan2020, Feb2020, Mar2020, Apr2020, May2020, Jun2020, `Controle Expiração`, Categorias)

strat_CDI_J48 <- strat_CDI_J48 %>% group_by(ITEM, `Controle Expiração`, Categorias) %>% summarise(Jul2019=sum(Jul2019),Aug2019=sum(Aug2019),Sep2019=sum(Sep2019),Oct2019=sum(Oct2019),Nov2019=sum(Nov2019),Dec2019=sum(Dec2019),
                                                                                            Jan2020=sum(Jan2020),Feb2020=sum(Feb2020),Mar2020=sum(Mar2020),Apr2020=sum(Apr2020),May2020=sum(May2020),Jun2020=sum(Jun2020))
strat_CDI_J48$SO_last12 <-rowSums(strat_CDI_J48[4:15])
strat_CDI_J48$mean_sold12 <-rowMeans(strat_CDI_J48[4:15])

#CDI F55
strat_CDI_F45 <- min_max %>% filter(ORG %in% c("F45","L05","L11"),Categorias=="Estoque estratégico apenas no CDI") %>% 
                              select (ORG, ITEM, inv_description, Jul2019, Aug2019, Sep2019, Oct2019, Nov2019, Dec2019,
                                      Jan2020, Feb2020, Mar2020, Apr2020, May2020, Jun2020, `Controle Expiração`, Categorias)

strat_CDI_F45 <- strat_CDI_F45 %>% group_by(ITEM, `Controle Expiração`, Categorias) %>% summarise(Jul2019=sum(Jul2019),Aug2019=sum(Aug2019),Sep2019=sum(Sep2019),Oct2019=sum(Oct2019),Nov2019=sum(Nov2019),Dec2019=sum(Dec2019),
                                                                                           Jan2020=sum(Jan2020),Feb2020=sum(Feb2020),Mar2020=sum(Mar2020),Apr2020=sum(Apr2020),May2020=sum(May2020),Jun2020=sum(Jun2020))
strat_CDI_F45$SO_last12 <-rowSums(strat_CDI_F45[4:15])
strat_CDI_F45$mean_sold12 <-rowMeans(strat_CDI_F45[4:15])

#mean considering only months with positive sales
strat_CDI_J48[strat_CDI_J48 == 0] <- NA
strat_CDI_J48$mean_sold12_diff0 <-rowMeans(strat_CDI_J48[4:15], na.rm = "TRUE")
strat_CDI_J48[is.na(strat_CDI_J48)] <- 0

strat_CDI_F45[strat_CDI_F45 == 0] <- NA
strat_CDI_F45$mean_sold12_diff0 <-rowMeans(strat_CDI_F45[4:15], na.rm = "TRUE")
strat_CDI_F45[is.na(strat_CDI_F45)] <- 0

#quantity of zeros and intermittence
strat_CDI_J48$qty_zero_12<-rowSums(strat_CDI_J48[4:15]==0)
strat_CDI_J48$intermittence_12=strat_CDI_J48$qty_zero_12/12*100  #intermittent = (qty of zeros/qty of months) * 100
strat_CDI_F45$qty_zero_12<-rowSums(strat_CDI_F45[4:15]==0)
strat_CDI_F45$intermittence_12=strat_CDI_F45$qty_zero_12/12*100  #intermittent = (qty of zeros/qty of months) * 100

#generating ESeg_init
#MIN-MAX - selecting full mean of months with positive sales / rounding THE VALUE
strat_CDI_J48 <- strat_CDI_J48 %>% mutate(ESeg_init = ifelse(intermittence_12<=30,round(mean_sold12),
                                                         ifelse(intermittence_12>30 & intermittence_12<90, round(mean_sold12_diff0),
                                                            ifelse(intermittence_12>=90 & intermittence_12<100,round(mean_sold12_diff0*.5001), 0))))

strat_CDI_F45 <- strat_CDI_F45 %>% mutate(ESeg_init = ifelse(intermittence_12<=30,round(mean_sold12),
                                                             ifelse(intermittence_12>30 & intermittence_12<90, round(mean_sold12_diff0),
                                                                    ifelse(intermittence_12>=90 & intermittence_12<100,round(mean_sold12_diff0*.5001), 0))))


#adding lead time
LT_J48 <- cooxupe_df %>% filter (ORG=="J48") %>% select (ITEM, inv_description, PREPROCESSING_LEAD_TIME, PROCESSING_LEAD_TIME, POSTPROCESSING_LEAD_TIME)
strat_CDI_J48 <- left_join(strat_CDI_J48, LT_J48)
strat_CDI_J48 <- strat_CDI_J48 %>% relocate(inv_description, .after = ITEM ) 
rm(LT_J48)
LT_F45 <- cooxupe_df %>% filter (ORG=="F45") %>% select (ITEM, inv_description, PREPROCESSING_LEAD_TIME, PROCESSING_LEAD_TIME,POSTPROCESSING_LEAD_TIME)
strat_CDI_F45 <- left_join(strat_CDI_F45, LT_F45)
strat_CDI_F45 <- strat_CDI_F45 %>% relocate(inv_description, .after = ITEM ) 
rm(LT_F45)

#Eseg_LT
strat_CDI_J48 <- strat_CDI_J48 %>% mutate(ESeg_LT = round(ESeg_init/21*(PREPROCESSING_LEAD_TIME+PROCESSING_LEAD_TIME)))
strat_CDI_F45 <- strat_CDI_F45 %>% mutate(ESeg_LT = round(ESeg_init/21*(PREPROCESSING_LEAD_TIME+PROCESSING_LEAD_TIME)))

#Eseg
strat_CDI_J48 <- strat_CDI_J48 %>% mutate(ESeg = ESeg_init)
strat_CDI_F45 <- strat_CDI_F45 %>% mutate(ESeg = ESeg_init)

######updating manual adjustments from Cooxupe Analysts
##additional out-of-scope ITEMS defined manually at the end of the project MODEL2  ##################
tempF45_1 <- read_excel("C:/Users/Juliano/Dropbox/Cooxupe/6_Planilhas_geradas/atualiz_manual_cooxupe/Cópia de strat_F45.xlsx", sheet  = "com histórico")
tempF45_2 <- read_excel("C:/Users/Juliano/Dropbox/Cooxupe/6_Planilhas_geradas/atualiz_manual_cooxupe/Cópia de strat_F45.xlsx", sheet  = "sem histórico - manual")
tempJ48_1 <- read_excel("C:/Users/Juliano/Dropbox/Cooxupe/6_Planilhas_geradas/atualiz_manual_cooxupe/Cópia de strat_CDI_J48.xlsx", sheet  = "com histórico")
tempJ48_2 <- read_excel("C:/Users/Juliano/Dropbox/Cooxupe/6_Planilhas_geradas/atualiz_manual_cooxupe/Cópia de strat_CDI_J48.xlsx", sheet  = "sem histórico - manual")

tempF45_1 <- tempF45_1 %>% filter (`Fora de Linha`!="Sim") %>% select(ITEM, ESeg)
tempF45_2 <- tempF45_2 %>% filter (Observação!="Inativar") %>% select(ITEM, ESeg)
tempJ48_1 <- tempJ48_1 %>% select(ITEM, ESeg)
tempJ48_2 <- tempJ48_2 %>% filter (Categorias!="Inativar") %>% select(ITEM, ESeg)
tempF45   <- full_join(tempF45_1, tempF45_2)
tempJ48   <- full_join(tempJ48_1, tempJ48_2)
#test_duplicate <- strat_CDI_J48 %>% group_by (ITEM) %>% filter (n()>1)
#test_duplicate <- tempF45 %>% group_by (ITEM) %>% filter (n()>1)

strat_CDI_F45 <- left_join(strat_CDI_F45,tempF45, by="ITEM")
strat_CDI_J48 <- left_join(strat_CDI_J48,tempJ48, by="ITEM")

strat_CDI_F45 <- strat_CDI_F45 %>% mutate(ESeg = ifelse(ESeg.x==0 & !is.na(ESeg.y), ESeg.y, ESeg.x))
strat_CDI_J48 <- strat_CDI_J48 %>% mutate(ESeg = ifelse(ESeg.x==0 & !is.na(ESeg.y), ESeg.y, ESeg.x))
rm(tempF45, tempF45_1, tempF45_2, tempJ48, tempJ48_1, tempJ48_2)

######updating manual 2nd spreadsheet from Cooxupe Analysts
##additional out-of-scope ITEMS defined manually at the end of the project MODEL2  ##################
temp <- read_excel("C:/Users/Juliano/Dropbox/Cooxupe/4_Planilhas_extraidas/Itens Flávio - voltar para CDI de MIN-MAX.xlsx", sheet  = "Itens")
temp <- temp %>% filter(ITEM %in% c(45608,1144,1863,8838,1866,1867,10357,3706))
temp[is.na(temp)] <- 0 #replace NA by  0
temp <- temp %>% select(ITEM, `Sugestão J48`)


strat_CDI_J48 <- left_join(strat_CDI_J48,temp, by="ITEM")
strat_CDI_J48 <- strat_CDI_J48 %>% mutate(ESeg = ifelse(`Sugestão J48` > 0 & !is.na(`Sugestão J48`), `Sugestão J48`, ESeg))
rm(temp)



#Files strategico CDI
strat_CDI_F45 <- strat_CDI_F45 %>% select (ITEM, inv_description, ESeg)
strat_CDI_J48 <- strat_CDI_J48 %>% select (ITEM, inv_description, ESeg)


#########################
#### Export to Excel ####
#########################
#Estrat. CDI
write_excel_csv(strat_CDI_J48,'C:/Users/Juliano/Dropbox/Cooxupe/6_Planilhas_geradas/strat_CDI_J48_1.csv', na = "NA", append = FALSE,
                delim = ",", quote_escape = "double")

write_excel_csv(strat_CDI_F45,'C:/Users/Juliano/Dropbox/Cooxupe/6_Planilhas_geradas/strat_CDI_F45_1.csv', na = "NA", append = FALSE,
                delim = ",", quote_escape = "double")


#########################
#### Export to Excel ####
#########################
#MIN-MAX
write_excel_csv(cooxupe_df,"C:/Users/Juliano/Dropbox/Cooxupe/6_Planilhas_geradas/cooxupe_df.csv", na = "NA", append = FALSE,
                delim = ",", quote_escape = "double")
write_excel_csv(cooxupe_df_long,"C:/Users/Juliano/Dropbox/Cooxupe/6_Planilhas_geradas/cooxupe_df_long.csv", na = "NA", append = FALSE,
                delim = ",", quote_escape = "double")
#file min-max stores
write_excel_csv(min_max_st,'C:/Users/Juliano/Dropbox/Cooxupe/6_Planilhas_geradas/min_max_lojas_0.csv', na = "NA", append = FALSE,
                delim = ",", quote_escape = "double")
write_excel_csv(min_max_1st_st,'C:/Users/Juliano/Dropbox/Cooxupe/6_Planilhas_geradas/min_max_lojas_1.csv', na = "NA", append = FALSE,
                delim = ",", quote_escape = "double")
write_excel_csv(min_max_2nd_st,'C:/Users/Juliano/Dropbox/Cooxupe/6_Planilhas_geradas/min_max_lojas_2.csv', na = "NA", append = FALSE,
                delim = ",", quote_escape = "double")
write_excel_csv(min_max_3rd_st,'C:/Users/Juliano/Dropbox/Cooxupe/6_Planilhas_geradas/min_max_lojas_3.csv', na = "NA", append = FALSE,
                delim = ",", quote_escape = "double")
write_excel_csv(min_max_CDI_J48,'C:/Users/Juliano/Dropbox/Cooxupe/6_Planilhas_geradas/min_max_CDI_J48.csv', na = "NA", append = FALSE,
                delim = ",", quote_escape = "double")
#file min-max UA
write_excel_csv(min_max_UA,'C:/Users/Juliano/Dropbox/Cooxupe/6_Planilhas_geradas/min_max_UA_0.csv', na = "NA", append = FALSE,
                delim = ",", quote_escape = "double")
write_excel_csv(min_max_1st_UA,'C:/Users/Juliano/Dropbox/Cooxupe/6_Planilhas_geradas/min_max_UA_1.csv', na = "NA", append = FALSE,
                delim = ",", quote_escape = "double")
write_excel_csv(min_max_2nd_UA,'C:/Users/Juliano/Dropbox/Cooxupe/6_Planilhas_geradas/min_max_UA_2.csv', na = "NA", append = FALSE,
                delim = ",", quote_escape = "double")
write_excel_csv(min_max_3rd_UA,'C:/Users/Juliano/Dropbox/Cooxupe/6_Planilhas_geradas/min_max_UA_3.csv', na = "NA", append = FALSE,
                delim = ",", quote_escape = "double")
write_excel_csv(min_max_CDI_F45,'C:/Users/Juliano/Dropbox/Cooxupe/6_Planilhas_geradas/min_max_CDI_F45.csv', na = "NA", append = FALSE,
                delim = ",", quote_escape = "double")
#Lista itens
write_excel_csv(general_itemlist,'C:/Users/Juliano/Dropbox/Cooxupe/6_Planilhas_geradas/item_list.csv', na = "NA", append = FALSE,
                delim = ",", quote_escape = "double")
