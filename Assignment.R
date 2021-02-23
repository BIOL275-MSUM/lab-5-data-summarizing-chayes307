
# New Packages ------------------------------------------------------------
library(tidyverse)


iris <- as_tibble(iris)


# Question #1 -------------------------------------------------------------

iris_pt2 <- rename(iris, sepal_length = Sepal.Length,
                   sepal_width = Sepal.Width, 
                   petal_length = Petal.Length,
                   petal_width =  Petal.Width,
                   species = Species)

# Question #2 -------------------------------------------------------------

a1 <- select(iris_pt2, sepal_length, sepal_width, petal_length, petal_width, species)
mutate(a1,  sepal_length = sepal_length * 10,
       sepal_width = sepal_width * 10, 
       petal_length = petal_length * 10, 
       petal_width = petal_width * 10)

# Question #3 -------------------------------------------------------------

a3 <- mutate(a1, sepal_area = sepal_length * sepal_width, 
             petal_area = petal_length * petal_width )
a4 <- select(a3, sepal_area, petal_area)

# Question 4 --------------------------------------------------------------
d2 <- select(a1, sepal_length)

summarize (a1,
           sampl_size= n(),
           max_sepal.length = max(sepal_length),
           min_sepal.length = min(sepal_length),
           range_sepal.length = range(max_sepal.length - min_sepal.length),         
           median_sepal_length = median(sepal_length),         
           first_quartile_sepal_length = quantile(sepal_length, probs = 0.25),
           IQR_sepal_length = IQR(sepal_length))


# Question 5 --------------------------------------------------------------
Petals_grouped <- group_by(iris, Petal.Width)
summarize (iris,
           sample_size = n(),
           mean = mean(Petal.Width),
           sd = sd(Petal.Width),
           var = var(Petal.Width),
           stderr = sd(Petal.Width) / sqrt(sample_size),
           ci_upper = mean + 2 * stderr,
           ci_lower = mean - 2* stderr
           
)

# Question #6 -------------------------------------------------------------

ggplot(data = a1) + geom_jitter(aes(y = petal_length, x = species)) 

# Question #7 -------------------------------------------------------------
ggplot(data = a1) + 
  geom_jitter(mapping =  aes( y = petal_width, x = species))
Width_summary <-
  summarize(
    Petals_grouped,
    petal_mean = mean(Petal.Width),
    petal_sem = mean(petal_mean) / sqrt(n()),
    ci_upper_limit = petal_mean + 1.96 * petal_sem,
    ci_lower_limit = petal_mean - 1.96 * petal_sem
  )
Width_summary




ggplot(data = a1) +
  geom_jitter(mapping = aes(x = species, y = petal_width), alpha = .1) +
  
  geom_crossbar(
    data = Width_summary, 
    mapping = aes(x = species, y = petal_mean, ymax = ci_upper_limit, 
                  ymin = ci_lower_limit),
    color = "red"
  )

# Question 8 --------------------------------------------------------------

ggplot(data = a1) +
  geom_point(mapping = aes(x = petal_length, y = petal_width, color = species), alpha = 0.1)
