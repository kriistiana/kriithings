library(readr)
library(lubridate)
library(dplyr)
library(magrittr)
library(stringr)
library(ggplot2)

bank_stmt <- Statement_adj

colnames(bank_stmt) <- c("transaction_date", "type", "description", "pmt_number", "bank_reference", "amount")

# Helper function

is_keyword_in<- function(column, keyword){
  is_string_in_word <- ifelse(max(str_detect(column,as.character(keyword))) > 0, 1, 0)
  return(is_string_in_word)
}

is_keyword_in('milapicalulu','lulu')
is_keyword_in('milapicalulu','ganbei')

# Keyword search

bank_stmt %<>%
  mutate(
    amount = abs(amount),
    transaction_date = dmy(transaction_date),
    transaction_week = week(transaction_date),
    transaction_month = month(transaction_date, label = T),
    transaction_yearmonth = floor_date(transaction_date,unit = 'month'),
    description_f = str_replace_all(tolower(gsub('[[:punct:]]|[[:digit:]]', '', description)), pattern=" ", repl=""),
    category = case_when(
      type %in% c('SKAIDRAS NAUDAS IZÒEMÐANA',
                  'SKAIDRAS NAUDAS IZ\xd2EM\xd0ANA')~ 'Cash withdrawal',
      type %in% c('IENÂKOÐAIS ZIBMAKSÂJUMS',
                  'IENÂKOÐAIS MAKSÂJUMS',
                  'IENÂKOÐAIS EKS MAKSÂJUMS',
                  'IEN\\\xc2KO\\\xd0AIS MAKS\\\xc2JUMS',
                  'IEN\\\xc2KO\\\xd0AIS ZIBMAKS\\\xc2JUMS') ~ 'Incoming',
      type %in% c('DEBETA PROCENTI', 'MAKSA PAR SK.NAUDAS IZÒEMÐANU', 'ÂTRÂ MAKSÂJUMA KOMISIJA') ~ 'Commisions',
      sapply(description_f,function(x) is_keyword_in(x,'wolt')) > 0 ~ 'Wolt',
      sapply(description_f,function(x) is_keyword_in(x,'bolt')) > 0 ~ 'Bolt',
      sapply(description_f,function(x) is_keyword_in(x,'wwwalv')) > 0 ~ 'Online shopping',
      sapply(description_f,function(x) is_keyword_in(x,'restart')) > 0 ~ 'Restart',
      sapply(description_f,function(x) is_keyword_in(x,'narvesen')) > 0 ~ 'Narvesen',
      sapply(description_f,function(x) is_keyword_in(x,'rimi')) > 0 ~ 'Rimi',
      sapply(description_f,function(x) is_keyword_in(x,'troubadour')) > 0 ~ 'Cadets',
      sapply(description_f,function(x) is_keyword_in(x,'cadets')) > 0 ~ 'Cadets',
      sapply(description_f,function(x) is_keyword_in(x,'audible')) > 0 ~ 'Audible',
      sapply(description_f,function(x) is_keyword_in(x,'applecombill')) > 0 ~ 'Apple',
      sapply(description_f,function(x) is_keyword_in(x,'circlek')) > 0 ~ 'Circle K',
      sapply(description_f,function(x) is_keyword_in(x,'wwwlululv')) > 0 ~ 'Lulu',
      sapply(description_f,function(x) is_keyword_in(x,'maxima')) > 0 ~ 'Maxima',
      sapply(description_f,function(x) is_keyword_in(x,'strawberrie')) > 0 ~ 'Ogas',
      sapply(description_f,function(x) is_keyword_in(x,'factilejeopardy')) > 0 ~ 'Factile',
      sapply(description_f,function(x) is_keyword_in(x,'terra')) > 0 ~ 'Terra',
      sapply(description_f,function(x) is_keyword_in(x,'ziedusalons')) > 0 ~ 'Flowers',
      sapply(description_f,function(x) is_keyword_in(x,'zieduveikals')) > 0 ~ 'Flowers',
      sapply(description_f,function(x) is_keyword_in(x,'vocalmaster')) > 0 ~ 'Singing',
      sapply(description_f,function(x) is_keyword_in(x,'bookingcom')) > 0 ~ 'Booking',
      sapply(description_f,function(x) is_keyword_in(x,'makonis')) > 0 ~ 'Makonis',
      sapply(description_f,function(x) is_keyword_in(x,'drogas')) > 0 ~ 'Drogas',
      sapply(description_f,function(x) is_keyword_in(x,'herbarijs')) > 0 ~ 'Herbarijs',
      sapply(description_f,function(x) is_keyword_in(x,'adobecom')) > 0 ~ 'Adobe',
      sapply(description_f,function(x) is_keyword_in(x,'amazonprime')) > 0 ~ 'Amazon',
      sapply(description_f,function(x) is_keyword_in(x,'galerijacentrshm')) > 0 ~ 'H&M',
      sapply(description_f,function(x) is_keyword_in(x,'siafitpeople')) > 0 ~ 'PeopleFitness',
      sapply(description_f,function(x) is_keyword_in(x,'gimlet')) > 0 ~ 'Gimlet',
      sapply(description_f,function(x) is_keyword_in(x,'stauere')) > 0 ~ 'Self Transfer',
      sapply(description_f,function(x) is_keyword_in(x,'thevillagecoconut')) > 0 ~ 'Vacation',
      sapply(description_f,function(x) is_keyword_in(x,'kiwi')) > 0 ~ 'Vacation',
      sapply(description_f,function(x) is_keyword_in(x,'danielakortina')) > 0 ~ 'Rent',
      sapply(description_f,function(x) is_keyword_in(x,'bangkok')) > 0 ~ 'Vacation',
      sapply(description_f,function(x) is_keyword_in(x,'stockman')) > 0 ~ 'Stockman',
      sapply(description_f,function(x) is_keyword_in(x,'elkor')) > 0 ~ 'Elkor',
      sapply(description_f,function(x) is_keyword_in(x,'rigalvatm')) > 0 ~ 'ATM',
      sapply(description_f,function(x) is_keyword_in(x,'travelstart')) > 0 ~ 'Vacation',
      sapply(description_f,function(x) is_keyword_in(x,'peekandcloppenburg')) > 0 ~ 'Peek and Cloppenburg',
      sapply(description_f,function(x) is_keyword_in(x,'paypalpowair')) > 0 ~ 'V airsoft',
      sapply(description_f,function(x) is_keyword_in(x,'paypalpaintball')) > 0 ~ 'V airsoft',
      sapply(description_f,function(x) is_keyword_in(x,'phuket')) > 0 ~ 'Vacation',
      sapply(description_f,function(x) is_keyword_in(x,'paypalsiaglamorallv')) > 0 ~ 'Amoralle',
      sapply(description_f,function(x) is_keyword_in(x,'jetair')) > 0 ~ 'Vacation',
      sapply(description_f,function(x) is_keyword_in(x,'intheair')) > 0 ~ 'Vacation',
      sapply(description_f,function(x) is_keyword_in(x,'steezystudio')) > 0 ~ 'Steezystudio',
      sapply(description_f,function(x) is_keyword_in(x,'morex')) > 0 ~ 'Online shopping',
      sapply(description_f,function(x) is_keyword_in(x,'ikea')) > 0 ~ 'Ikea',
      sapply(description_f,function(x) is_keyword_in(x,'airport')) > 0 ~ 'Vacation',
      sapply(description_f,function(x) is_keyword_in(x,'arlandatp')) > 0 ~ 'Vacation',
      sapply(description_f,function(x) is_keyword_in(x,'wowbeauty')) > 0 ~ 'WOW',
      sapply(description_f,function(x) is_keyword_in(x,'paypalcdkeys')) > 0 ~ 'Games',
      sapply(description_f,function(x) is_keyword_in(x,'paypaljonathanl')) > 0 ~ 'Poker',
      sapply(description_f,function(x) is_keyword_in(x,'wwwasoscom')) > 0 ~ 'Asos',
      sapply(description_f,function(x) is_keyword_in(x,'paypalroaringtop')) > 0 ~ 'V airsoft',
      sapply(description_f,function(x) is_keyword_in(x,'veikalssportsdirect')) > 0 ~ 'Shopping',
      sapply(description_f,function(x) is_keyword_in(x,'mistersbobs')) > 0 ~ 'Lunch at work',
      sapply(description_f,function(x) is_keyword_in(x,'kafejnicasala')) > 0 ~ 'Lunch at work',
      sapply(description_f,function(x) is_keyword_in(x,'hsreplaynet')) > 0 ~ 'Subscriptions',
      sapply(description_f,function(x) is_keyword_in(x,'yandex')) > 0 ~ 'Yandex',
      sapply(description_f,function(x) is_keyword_in(x,'labaspreces')) > 0 ~ 'LabasPreces',
      sapply(description_f,function(x) is_keyword_in(x,'peterclarke')) > 0 ~ 'Poker',
      sapply(description_f,function(x) is_keyword_in(x,'manslmt')) > 0 ~ 'PhoneBill',
      sapply(description_f,function(x) is_keyword_in(x,'indexo')) > 0 ~ 'Pension',
      sapply(description_f,function(x) is_keyword_in(x,'lakstos')) > 0 ~ 'Flowers',
      sapply(description_f,function(x) is_keyword_in(x,'frizetava')) > 0 ~ 'Hairdresser',
      sapply(description_f,function(x) is_keyword_in(x,'mango')) > 0 ~ 'Mango',
      sapply(description_f,function(x) is_keyword_in(x,'zimolaveikals')) > 0 ~ 'Gifts',
      sapply(description_f,function(x) is_keyword_in(x,'amoralle')) > 0 ~ 'Amoralle',
      sapply(description_f,function(x) is_keyword_in(x,'icard')) > 0 ~ 'Poker',
      sapply(description_f,function(x) is_keyword_in(x,'upswingpoker')) > 0 ~ 'Poker',
      sapply(description_f,function(x) is_keyword_in(x,'cardrunnersev')) > 0 ~ 'Poker',
      sapply(description_f,function(x) is_keyword_in(x,'gmtbeauty')) > 0 ~ 'Gifts',
      sapply(description_f,function(x) is_keyword_in(x,'sexystyle')) > 0 ~ 'Sexystyle',
      sapply(description_f,function(x) is_keyword_in(x,'zaraveikals')) > 0 ~ 'Zara',
      sapply(description_f,function(x) is_keyword_in(x,'vinastudija')) > 0 ~ 'Gifts',
      sapply(description_f,function(x) is_keyword_in(x,'lashandbrow')) > 0 ~ 'Beauty',
      sapply(description_f,function(x) is_keyword_in(x,'pokercoaching')) > 0 ~ 'Poker',
      sapply(description_f,function(x) is_keyword_in(x,'elgarsozolins')) > 0 ~ 'Rent',
      sapply(description_f,function(x) is_keyword_in(x,'accessorize')) > 0 ~ 'Accessorize',
      sapply(description_f,function(x) is_keyword_in(x,'mango')) > 0 ~ 'Mango',
      sapply(description_f,function(x) is_keyword_in(x,'douglas')) > 0 ~ 'Douglas',
      sapply(description_f,function(x) is_keyword_in(x,'bilesuserviss')) > 0 ~ 'Bilesuserviss',
      sapply(description_f,function(x) is_keyword_in(x,'ledusskola')) > 0 ~ 'LedusSkola',
      sapply(description_f,function(x) is_keyword_in(x,'psxlv')) > 0 ~ 'Gifts',
      sapply(description_f,function(x) is_keyword_in(x,'aseimriga')) > 0 ~ 'Apavi40plus',
      sapply(description_f,function(x) is_keyword_in(x,'ilgvarslaganovskis')) > 0 ~ 'Transfers',
      sapply(description_f,function(x) is_keyword_in(x,'galleriarigahm')) > 0 ~ 'H&M',
      sapply(description_f,function(x) is_keyword_in(x,'foodfactory')) > 0 ~ 'FoodFactory',
      sapply(description_f,function(x) is_keyword_in(x,'einarsbirkhans')) > 0 ~ 'DrivingLessons',
      sapply(description_f,function(x) is_keyword_in(x,'akropole')) > 0 ~ 'Akropole',
      sapply(description_f,function(x) is_keyword_in(x,'domina')) > 0 ~ 'Domina',
      sapply(description_f,function(x) is_keyword_in(x,'valtersunrapa')) > 0 ~ 'Books',
      sapply(description_f,function(x) is_keyword_in(x,'janisroze')) > 0 ~ 'Books',
      sapply(description_f,function(x) is_keyword_in(x,'crushlive')) > 0 ~ 'Poker',
      
      T ~ 'Other'
    ),

    
    category_main = case_when(category %in% c('Rimi','Maxima','Ogas', 'Drogas','Flowers') ~ 'Groceries',
                              category %in% c('Restart','Terra') ~ 'Lunch at work',
                              category %in% c('Circle K','Narvesen','Cadets') ~ 'Convenience shops',
                              category %in% c('Wolt','Lulu','FoodFactory') ~ 'Food delivery',
                              category %in% c('Makonis', 'Gimlet','Herbarijs') ~ 'Bars',
                              category %in% c('Adobe','Factile','Apple','PeopleFitness','Steezystudio','Amazon','Audible') ~ 'Subscriptions',
                              category %in% c('Elkor','Stockman','Peek and Cloppenburg', 'Amoralle','Online shopping',
                                              'Ikea',
                                              'Games',
                                              'Asos',
                                              "H&M",
                                              'Mango',
                                              'Sexystyle',
                                              'Zara',
                                              'Douglas',
                                              'Accessorize',
                                              'Apavi40plus',
                                              'Akropole',
                                              'Domina'
                                              ) ~ 'Shopping',
                              category %in% c('Bolt', 'Yandex') ~ 'Taxi',
                              category %in% c('LabasPreces') ~ 'Home',
                              category %in% c('Bilesuserviss','Booking') ~ 'Culture&Relax',
                              category %in% c('Hairdresser') ~ 'Beauty',
                              category %in% c('LedusSkola','DrivingLessons','Books') ~ 'Self-Development',
                              
                              T ~ category)
    ) 

# How many gave you categorized so far?

table(bank_stmt$category)/nrow(bank_stmt)


# In the process of keyword searching, you want to refine categories so that "Other" contains less 

bank_stmt %>% filter(category == 'Other') %>% arrange(desc(amount)) %>% select(description_f,description, type, amount) %>% as_tibble() %>%  View()
