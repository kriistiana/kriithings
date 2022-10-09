library(devtools)
library(woe)
library(InformationValue)
library(gridExtra)
library(dplyr)
library(ggplot2)
library(Hmisc)

infvalue <- create_infotables(data=invoices_clean, y="response", bins=10, parallel=FALSE)

infvalue$Summary %>% View()

infvalue$Tables$payer_age_num


# payer_risika_score
IV(X=factor(invoices_clean$payer_risika_score,
               levels = c("?","0.0","1.0", "2.0","3.0","4.0","5.0","6.0","7.0","8.0","9.0","10.0")),
   Y=invoices_clean$response,valueOfGood = 1)

wt <- WOETable(X=factor(invoices_clean$payer_risika_score,
            levels = c("?","0.0","1.0", "2.0","3.0","4.0","5.0","6.0","7.0","8.0","9.0","10.0")),
   Y=invoices_clean$response,valueOfGood = 1)

gridExtra::grid.arrange(wt %>% mutate(br = GOODS/TOTAL) %>% 
                          ggplot(aes(as.numeric(CAT),br)) + 
                          geom_point() + 
                          xlab("Badrate") +
                          geom_label(aes(y = br + 0.05,label = round(br,3))) +
                          scale_x_continuous("Badrate", breaks = 0:10),
                        
                        wt %>% mutate(distr = TOTAL/sum(TOTAL)) %>% 
                          ggplot(aes(as.numeric(CAT),distr)) + 
                          geom_bar(stat = "identity") + 
                          xlab("Distribution") +
                          geom_label(aes(y = distr + 0.1,label = round(distr,3))) +
                          scale_x_continuous("Distribution", breaks = 0:10))

# payer_risika_score
IV(X=factor(invoices_clean$seller_risika_score,
            levels = c("?","0.0","1.0", "2.0","3.0","4.0","5.0","6.0","7.0","8.0","9.0","10.0")),
   Y=invoices_clean$response,valueOfGood = 1)

wt <- WOETable(X=factor(invoices_clean$seller_risika_score,
                        levels = 0:10),
               Y=invoices_clean$response,valueOfGood = 1)

gridExtra::grid.arrange(wt %>% mutate(br = GOODS/TOTAL) %>% 
                          ggplot(aes(as.numeric(CAT),br)) + 
                          geom_point() + 
                          geom_label(aes(y = br + 0.1,label = round(br,3))) +
                          scale_x_continuous("Badrate", breaks = 0:10) +
                          ylim(0,0.2),
                        
                        wt %>% mutate(distr = TOTAL/sum(TOTAL)) %>% 
                          ggplot(aes(as.numeric(CAT),distr)) + 
                          geom_bar(stat = "identity") + 
                          geom_label(aes(y = distr + 0.05,label = round(distr,3))) +
                          scale_x_continuous("Distribution", breaks = 0:10))


# offer_seller_is_first_invoice
IV(X=factor(invoices_clean$offer_seller_is_first_invoice),
   Y=invoices_clean$response,valueOfGood = 1)

wt <- WOETable(X=factor(invoices_clean$offer_seller_is_first_invoice),
         Y=invoices_clean$response,valueOfGood = 1)%>% mutate(br = GOODS/TOTAL)

gridExtra::grid.arrange(wt %>% mutate(br = GOODS/TOTAL) %>% 
                          ggplot(aes(CAT,br)) + 
                          geom_point() + 
                          xlab("Badrate") +
                          geom_label(aes(y = br + 0.005,label = round(br,3))),
                        
                        wt %>% mutate(distr = TOTAL/sum(TOTAL)) %>% 
                          ggplot(aes(CAT,distr)) + 
                          geom_bar(stat = "identity") + 
                          xlab("Distribution") +
                          geom_label(aes(y = distr + 0.1,label = round(distr,3))))


# payer_comp_type_f
IV(X=factor(invoices_clean$payer_comp_type_f),
   Y=invoices_clean$response,valueOfGood = 1)

pp %>% mutate(br = GOODS/TOTAL) %>% ggplot(aes(as.numeric(CAT),br))+geom_point()

gridExtra::grid.arrange(pp %>% mutate(br = GOODS/TOTAL) %>% ggplot(aes(as.numeric(CAT),br)) + geom_point() + xlab("Badrate"),
                        pp %>% mutate(distr = TOTAL/sum(TOTAL)) %>% ggplot(aes(as.numeric(CAT),distr)) + geom_bar(stat = "identity") + xlab("Distribution"))
WOETable(X=factor(invoices_clean$payer_comp_type_f),
         Y=invoices_clean$response,valueOfGood = 1)%>% mutate(br = GOODS/TOTAL)

# payer_industry_size_f
IV(X=factor(invoices_clean$payer_industry_size_f),
   Y=invoices_clean$response,valueOfGood = 1)

WOETable(X=factor(invoices_clean$seller_industry_size_f),
         Y=invoices_clean$response,valueOfGood = 1)%>% mutate(br = GOODS/TOTAL)

# seller_industry_size_f
IV(X=factor(invoices_clean$seller_industry_size_f),
   Y=invoices_clean$response,valueOfGood = 1)

WOETable(X=factor(invoices_clean$seller_industry_size_f),
         Y=invoices_clean$response,valueOfGood = 1)%>% mutate(br = GOODS/TOTAL)

# offer_seller_is_first_invoice

IV(X=factor(invoices_clean$offer_seller_is_first_invoice),
   Y=invoices_clean$response,valueOfGood = 1)

WOETable(X=factor(invoices_clean$offer_seller_is_first_invoice),
         Y=invoices_clean$response,valueOfGood = 1)%>% mutate(br = GOODS/TOTAL)

# offer_seller_is_first_invoice

IV(X=factor(invoices_clean$payer_age_num),
   Y=invoices_clean$response,valueOfGood = 1)

WOETable(X=factor(invoices_clean$payer_age_num),
         Y=invoices_clean$response,valueOfGood = 1)%>% mutate(br = GOODS/TOTAL)










woe(Data=invoices_clean,
    Independent = "invoice_amount",
    Continuous = T,
    Dependent = "response",
    C_Bin = 10,
    Bad = 1,
    Good = 0
    )


t<-factor(invoices_clean$payer_risika_score,
          levels = c("?" ,   "0.0",  "1.0",  "10.0", "2.0" , "3.0" , "4.0" , "5.0" , "6.0",  "7.0" , "8.0"  ,"9.0"))
