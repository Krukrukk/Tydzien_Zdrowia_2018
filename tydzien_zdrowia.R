# Maciej Ma³ecki
library(dplyr)
library(ggplot2)
library(readr)
library(scales)
library(rlang)

Tydzien_Zdrowia_origin <- read_delim("G:/Dysk Googla/tydzien_zdrowia/Tydzien_Zdrowia_Odpowiedzi.csv", 
                                         ";", escape_double = FALSE, trim_ws = TRUE)

df_TZ <- Tydzien_Zdrowia_origin
View(Tydzien_Zdrowia_origin)

#   #   #   #   #   #   #   #   #   #    Preprocesing     #   #   #   #   #   #   #   #   #   #

### Change numeric col on factor
factor_index <- c(2:5,8:14, 16:17, 19:25, 27:28, 30:34)
for (i in factor_index){
    df_TZ[,i] <- as.factor(unlist(df_TZ[,i])) 
}

### Delete data col
df_TZ <- df_TZ[,-1]

### Change label of column
names(df_TZ) <- c('Plec', 'Wydzial', 'Rok', 'Zaangazowanie_studenckie', 'Wzrost','Wiek', 'Stres',
                    'Praca','Czas_na_uczelni', 'Mieszkanie', 'Dojazd', 'Reguralna_nauka', 
                    'Nauka_godz', 'Srednia', 'Utrzymanie', 'Sposob_bycia', 'Woda', 'Kawa',
                    'Energetyki','Palenie', 'Uzaleznienia','Alkohol', 'Miejsce_jedzenia','Jedzenie',
                    'Aktywnosc', 'Regularnosc_aktywnosci', 'Okulary','Spanie', 'Zadowolenie', 'Zdrowie',
                    'Suplementy', 'Alergia', 'Czas_Kontuzji')



df <- df_TZ

theme_set(theme_classic())


#   #   #   #   #   #   #   #   #   #   #   #   #   # Analysis #    #   #   #   #   #   #   #   #   #   #   #


#   #   #   #   #   #   #   #   Pie chart for ones variable

pie_function <- function(data, column){
    column <- sym(column)
    columnn <- enquo(column)
    value <-  data %>%
        group_by(!!columnn) %>%
        summarise(count_row = length(!!columnn))
    
    pie <- ggplot(value, aes(x="", y=count_row, fill=!!columnn))+
        geom_bar(width = 1, stat="identity") +
        theme(axis.line = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              plot.title = element_text(hjust = 0.5, color = "#666666")) +
        geom_text(aes(label = paste0(round(count_row), "%")), position = position_stack(vjust = 0.5))+
        labs(fill=columnn,
             x=NULL,
             y=NULL,
             title=paste("Pie Chart of", deparse(substitute(column))),
             caption="Source: Tydzieñ Zdrowia")+
        coord_polar(theta="y")
    return(pie)
}

factors <- df[sapply(df, is.factor)]
factors_colnames <- colnames(factors)

#   #   # Solution
piee <- pie_function(factors,factors_colnames[12])
piee


#   #   #   #   #   #   #   #   #   wada wzroku a sen

df %>%
    filter(Spanie<12) %>%
    mutate(Okulary=fct_reorder(Okulary, Spanie, fun=median)) %>%
    ggplot(aes(x=reorder(Okulary, Spanie), y=Spanie, fill=Okulary))+
    geom_boxplot()+
    xlab('Okulary')+
    theme(legend.position="none")
### Osoby z wad¹ wzroku nie œpi¹ d³u¿ej ni¿ 8h



#   #   #   #   #   #   #   #   #   wada wzroku a kontuzje
df %>%
    filter(Aktywnosc<15) %>%
    mutate(Okulary=fct_reorder(Okulary, Aktywnosc, fun=median)) %>%
    ggplot(aes(x=reorder(Okulary, Aktywnosc), y=Aktywnosc, fill=Okulary))+
    geom_boxplot()+
    xlab('Okulary')+
    theme(legend.position="none")

#   #   #   #   #   #   #   #   #   wada wzroku a wydzia³
df %>%
    ggplot(aes(Wydzial))+
    geom_bar(aes(fill= Okulary))+
    theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
    labs(title="Histogram on Categorical Variable", 
         caption = "Sourse: Tydzieñ zdrowia",
         x = "Wydzia³",
         y = " ",
         fill = "Czy nosisz okulary?") 


#   #   #   #   #   #   #   #   #   trenowanie w klubie a kto utrzymuje
df %>%
    filter(Aktywnosc>3) %>%
    ggplot(aes(Regularnosc_aktywnosci))+
    geom_bar(aes(fill= Utrzymanie))+
    theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
    labs(title="Histogram on Categorical Variable", 
         caption = "Sourse: Tydzieñ zdrowia",
         x = "Regularna Aktywnoœæ",
         y = " ",
         fill = "Rodzaj utrzymania") 

#   #   #   #   #   #   #   #   #   Miejsce_jedzienia a kto utrzymuje
df %>%
    ggplot(aes(Miejsce_jedzenia))+
    geom_bar(aes(fill= Utrzymanie))+
    theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
    labs(title="Histogram on Categorical Variable", 
         caption = "Sourse: Tydzieñ zdrowia",
         x = "Miejsce spo¿ywania posi³ków",
         y = " ",
         fill = "Rodzaj utrzymania") 
### Osoby jedz¹ce mro¿onki nie dostaj¹ stypendiów 



#   #   #   #   #   #   #   #   #   Miejsce_jedzienia a jedzienie u osób aktywnych
df %>%
    filter(Aktywnosc>5 & Aktywnosc<15)%>%
    ggplot(aes(Miejsce_jedzenia))+
    geom_bar(aes(fill= Jedzenie))+
    theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
    labs(title="Histogram on Categorical Variable",
         subtitle = "Rodzaj posi³ków u osób aktywnych",
         caption = "Sourse: Tydzieñ zdrowia",
         x = "Miejsce spo¿ywania posi³ków",
         y = " ",
         fill = "Rodzaj diety") 
### Osoby aktywne jadaj¹ tylko œwie¿e posi³ki w domu i na mieœcie



# Wysypianie siê a wydzia³

df %>%
    mutate(Spanie_Z = round((Spanie - mean(Spanie))/sd(Spanie), 2))%>%
    group_by(Wydzial) %>%
    summarise(Mean_spanie_z = mean(Spanie_Z, na.rm=TRUE)) %>%
    arrange(desc(Mean_spanie_z)) %>%
    mutate(Spanie_type = ifelse(Mean_spanie_z < 0 , "Poni¿ej", "Powy¿ej")) %>%
    ggplot(aes(x=Wydzial, y=Mean_spanie_z, label=Mean_spanie_z))+
    geom_bar(stat='identity', aes(fill=Spanie_type), width = 0.5)+
    scale_fill_manual(name="Poziom wyspania", 
                      labels=c("Poni¿ej œredniej", "Powy¿ej œredniej"),
                      values = c("Powy¿ej"="#00ba38", "Poni¿ej"="#f8766d"))+
    labs(subtitle="Normalised Sen from 'Tydzieñ zdrowia'", 
         title= "Diverging Bars") + 
    coord_flip()


df %>%
    mutate(Spanie_Z = round((Spanie - mean(Spanie))/sd(Spanie), 2))%>%
    group_by(Wydzial) %>%
    summarise(Mean_spanie_z = mean(Spanie_Z, na.rm=TRUE)) %>%
    arrange(desc(Mean_spanie_z)) %>%
    ggplot(aes(x=Wydzial, y=Mean_spanie_z, label=Mean_spanie_z))+ 
    geom_point(stat='identity', fill="black", size=6)  +
    geom_segment(aes(y = 0, 
                     x = Wydzial, 
                     yend = Mean_spanie_z, 
                     xend = Wydzial), 
                 color = "black") +
    geom_text(color="white", size=2) +
    theme_bw()+
    labs(title="Diverging Lollipop Chart", 
         subtitle="Normalized Normalised Sen from 'Tydzieñ zdrowia': Lollipop") + 
    ylim(-1, 1) +
    coord_flip()



#   #   #   #   #   #   #   #   #   #   Palenie a stan zdrowia
df %>%
    ggplot(aes(Palenie))+
    geom_bar(aes(fill= Zdrowie))+
    theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
    labs(title="Histogram on Categorical Variable", 
         caption = "Sourse: Tydzieñ zdrowia",
         x = "Jak czêsto palisz?",
         y = " ",
         fill = "Stan zdrowia?") 
### Osoby, które mniej pal¹, uwa¿aj¹ swój stan zdrowia za lepszy

#   #   #   #   #   #   #   #   #   #   Miejsce zamieszkania a dojazd na uczelnie
df %>%
    ggplot(aes(Mieszkanie))+
    geom_bar(aes(fill= Dojazd))+
    theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
    labs(title="Histogram on Categorical Variable", 
         caption = "Sourse: Tydzieñ zdrowia",
         x = "Miejsce zamieszkania?",
         y = " ",
         fill = "Rodzaj transportu?") 
### Osoby mieszkaj¹ce w akademikach nie je¿d¿¹ samochodami

#   #   #   #   #   #   #   #   Wzrost a stres
df %>%
    mutate(Okulary=fct_reorder(Stres, Wzrost, fun=median)) %>%
    ggplot(aes(x=reorder(Stres, Wzrost), y=Wzrost, fill=Stres))+
    geom_boxplot()+
    xlab('Regularnosc aktywnosci')+
    theme(legend.position="none")
### Im ni¿sza osoba tym bardziej siê stresuje



#   #   #   #   #   #   #   #   Wzrost a wydzia³
df %>%
    mutate(Wzrost_Z = round((Wzrost - mean(Wzrost))/sd(Wzrost), 2))%>%
    group_by(Wydzial) %>%
    summarise(Mean_Wzrost_z = mean(Wzrost_Z, na.rm=TRUE)) %>%
    arrange(desc(Mean_Wzrost_z)) %>%
    mutate(Wzrost_type = ifelse(Mean_Wzrost_z < 0 , "Poni¿ej", "Powy¿ej")) %>%
    ggplot(aes(x=Wydzial, y=Mean_Wzrost_z, label=Mean_Wzrost_z))+
    geom_bar(stat='identity', aes(fill=Wzrost_type), width = 0.5)+
    scale_fill_manual(name="Wzrost", 
                      labels=c("Poni¿ej œredniej", "Powy¿ej œredniej"),
                      values = c("Powy¿ej"="#00ba38", "Poni¿ej"="#f8766d"))+
    labs(subtitle="Normalised Wzrost from 'Tydzieñ zdrowia'", 
         title= "Diverging Bars") + 
    coord_flip()
### To jest ciekawe, wydzia³y na których wiêcej siê siedzi przy biurku maj¹ wiêcej ni¿szych osób
