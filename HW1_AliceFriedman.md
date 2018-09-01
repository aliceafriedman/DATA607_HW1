Mushrooms HW1
================
Alice Friedman
09/01/2018

DATA 607, Assignment 1
----------------------

Libraries loaded: plyr, stringr, tidyverse, rehape2

``` r
## Load mushroom data from URL
mushrooms <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"), header=TRUE)

## Check data
head(mushrooms)
```

    ##   p x s n t p.1 f c n.1 k e e.1 s.1 s.2 w w.1 p.2 w.2 o p.3 k.1 s.3 u
    ## 1 e x s y t   a f c   b k e   c   s   s w   w   p   w o   p   n   n g
    ## 2 e b s w t   l f c   b n e   c   s   s w   w   p   w o   p   n   n m
    ## 3 p x y w t   p f c   n n e   e   s   s w   w   p   w o   p   k   s u
    ## 4 e x s g f   n f w   b k t   e   s   s w   w   p   w o   e   n   a g
    ## 5 e x y y t   a f c   b n e   c   s   s w   w   p   w o   p   k   n g
    ## 6 e b s w t   a f c   b g e   c   s   s w   w   p   w o   p   k   n m

``` r
## Create data frame
mushrooms <- as.data.frame(mushrooms)

##subset columns for class, cap-color, odor, and habitat
shrooms <- mushrooms[,c("p","n","p.1","u")]

## Check data
head(shrooms)
```

    ##   p n p.1 u
    ## 1 e y   a g
    ## 2 e w   l m
    ## 3 p w   p u
    ## 4 e g   n g
    ## 5 e y   a g
    ## 6 e w   a m

``` r
##rename columns using plyr
shrooms <- rename(shrooms, c("p"="poison_class","n"="cap_color","p.1"="odor","u"="habitat"))

##check levels, missing values in shrooms#poison_class
levels(shrooms$poison_class)
```

    ## [1] "e" "p"

``` r
is.null(shrooms$poison_class)
```

    ## [1] FALSE

``` r
##rename levels using str_detect (in stringr package) and ifelse 
##Note: this only works because there are only two levels
shrooms$poison_class <- ifelse(str_detect(shrooms$poison_class,"e")== TRUE, "edible", "poisonous")

##repeat for remaining columns, levels
levels(shrooms$cap_color)
```

    ##  [1] "b" "c" "e" "g" "n" "p" "r" "u" "w" "y"

``` r
##result: too many levels for ifelse! need to use gsub with a regexp, instead
shrooms$cap_color <- gsub("^b$","buff",shrooms$cap_color)
shrooms$cap_color <- gsub("^e$","red",shrooms$cap_color)
shrooms$cap_color <- gsub("^n$","brown",shrooms$cap_color)
shrooms$cap_color <- gsub("^c$","cinnamon",shrooms$cap_color)
shrooms$cap_color <- gsub("^g$","grey",shrooms$cap_color)
shrooms$cap_color <- gsub("^r$","green",shrooms$cap_color)
shrooms$cap_color <- gsub("^w$","white",shrooms$cap_color)
shrooms$cap_color <- gsub("^p$","pink",shrooms$cap_color)
shrooms$cap_color <- gsub("^y$","yellow",shrooms$cap_color)
shrooms$cap_color <- gsub("^u$","purple",shrooms$cap_color)

##gsub will force to character, change back to factor for easy checking work
shrooms$cap_color <- as.factor(shrooms$cap_color)

##repeat steps above for odor and habitat
shrooms$odor <- gsub("^a$","almond",shrooms$odor)
shrooms$odor <- gsub("^l$","anise",shrooms$odor)
shrooms$odor <- gsub("^c$","creosote",shrooms$odor)
shrooms$odor <- gsub("^y$","fishy",shrooms$odor)
shrooms$odor <- gsub("^f$","foul",shrooms$odor)
shrooms$odor <- gsub("^m$","musty",shrooms$odor)
shrooms$odor <- gsub("^n$","none",shrooms$odor)
shrooms$odor <- gsub("^p$","pungent",shrooms$odor)
shrooms$odor <- gsub("^s$","spicy",shrooms$odor)

shrooms$odor <- as.factor(shrooms$odor)

shrooms$habitat <- gsub("^d$","woods",shrooms$habitat)
shrooms$habitat <- gsub("^w$","waste",shrooms$habitat)
shrooms$habitat <- gsub("^l$","leaves",shrooms$habitat)
shrooms$habitat <- gsub("^g$","grasses",shrooms$habitat)
shrooms$habitat <- gsub("^m$","meadows",shrooms$habitat)
shrooms$habitat <- gsub("^p$","paths",shrooms$habitat)
shrooms$habitat <- gsub("^u$","urban",shrooms$habitat)

shrooms$habitat <- as.factor(shrooms$habitat)

## Check data
head(shrooms)
```

    ##   poison_class cap_color    odor habitat
    ## 1       edible    yellow  almond grasses
    ## 2       edible     white   anise meadows
    ## 3    poisonous     white pungent   urban
    ## 4       edible      grey    none grasses
    ## 5       edible    yellow  almond grasses
    ## 6       edible     white  almond meadows

``` r
tail(shrooms)
```

    ##      poison_class cap_color  odor habitat
    ## 8118    poisonous     brown  foul   woods
    ## 8119       edible     brown  none  leaves
    ## 8120       edible     brown  none  leaves
    ## 8121       edible     brown  none  leaves
    ## 8122    poisonous     brown fishy  leaves
    ## 8123       edible     brown  none  leaves

Graphic exloration of data
--------------------------

Graphing poison\_class vs. characteristics subsetted

``` r
#poison_class vs. odor
poison_by_odor <-table(shrooms[c(1,3)])
barplot(poison_by_odor,legend.text=TRUE, beside=TRUE, col=c("light blue","dark orange"), xlab = "Odort",ylab = "Species Count", main="Frequency of Poisonous Mushrooms by Odor")
```

![](HW1_AliceFriedman_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
#poison_class vs. habitat
poison_by_habitat <-table(shrooms[c(1,4)])
barplot(poison_by_habitat,legend.text=TRUE, beside=TRUE, col=c("light blue","dark orange"), xlab = "Habitat",ylab = "Species Count", main="Frequency of Poisonous Mushrooms by Habitat")
```

![](HW1_AliceFriedman_files/figure-markdown_github/unnamed-chunk-2-2.png)

``` r
#poison_class vs. cap_color
poison_by_habitat <-table(shrooms[c(1,2)])
barplot(poison_by_habitat,legend.text=TRUE, beside=TRUE, col=c("light blue","dark orange"), xlab = "Cap Color",ylab = "Species Count", main="Frequency of Poisonous Mushrooms by Cap Color")
```

![](HW1_AliceFriedman_files/figure-markdown_github/unnamed-chunk-2-3.png)

``` r
#fancy color scale  using ggplot
cap_color <- table(shrooms[2])
cap_color_melt <- melt(cap_color)
cap_color_melt <- as.data.frame(cap_color_melt)
cap_color_melt <- cap_color_melt %>% arrange(value)

data=data.frame(
  id=c(1:10),
  individual=cap_color_melt[1],
  value=cap_color_melt[2]
)
colnames(data) <-c("id","individual","value")

# Make the plot
p = ggplot(data, aes(x=as.factor(individual), y=value))+
    geom_bar(stat="identity", aes(fill=individual))+
    #Note: colors will be pulled in alphapbetical order
    scale_fill_manual("legend", values = c( 
      "#8B4513", #brown
      "#f0dc82", #buff
      "#9d4535", #cinnamon
      "#008000", #green
      "gray", #grey
      "#FFC0CB", #pink
      "purple", #purple
      "red", #red
      "white", #white 
      "#FFFF99" #yellow
      )
    )+
  ggtitle("Mushroom Species Frequency by Cap Colors")+
  labs(y="Number of Species", x = "Cap Color")

p
```

![](HW1_AliceFriedman_files/figure-markdown_github/unnamed-chunk-3-1.png)
