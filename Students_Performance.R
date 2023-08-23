---
title: "Exercise Sheet 3"
fontsize: 11pt
header-includes: \usepackage[german]{babel}
output:
  html_document: default
  pdf_document:
    highlight: tango
fig_caption: yes
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, # -> Sollen Code Chunks im gerenderten Dokument angezeigt werden?
                      eval = TRUE, # -> Sollen R Code Chunks ausgeführt werden?
                      warning = FALSE, # -> Warnungen sollten nur am Ende zum Rendern auf FALSE gesetzt werden
                      message = FALSE) # -> Hinweise sollten nur am Ende zum Rendern auf FALSE gesetzt werden
```

```{r}
# Set up libraries (make sure they are installed, first)
library(tidyverse)
library(stringr)
library(magrittr)
library(cluster)
library(dbscan)

```

1. A school would like to group its pupils according to their performance at two intermediate examinations. It is assumed that there are at least 2 clusters of pupils. Load the file `clustering-student-mat.csv` from the exercise sheet's ZIP archive. The file contains for each of the two exams the number of points scored for a total of 395 students.  
Perform a $K$-means-Clustering for each $k\in \{2,3,\ldots,8\}$. Display the cluster assignments of the points in a scatter plot. (You may use `kmeans` from package `cluster`/`stats`.)

```{r}
# Solution of task 1...
student <- read_csv("clustering-student-mat.csv")

K <- 2:8
# The tilde creates a formula and the dot represents a placeholder for the (single) argument K to define the number of centers/clusters
list_clu_res_k <- map(K, ~kmeans(student, centers = .))

for(i in seq_along(K)) {
  student_clu_k <- student %>%
    add_column(cluster = list_clu_res_k[[i]]$cluster) %>%
    mutate(cluster = factor(cluster))
  
  print(ggplot(student_clu_k, aes(Exam1, Exam2, color = cluster, fill = cluster)) +
    geom_point(pch = 21) +
    geom_point(data = student_clu_k %>%
        group_by(cluster) %>%
        summarize_all(mean), shape = "+", color = "black",
      size = 8) +
    guides(fill = "none", color = "none") +
    labs(title = str_c("k=", K[i])))
}

```

2. Aside from distance-based clustering models, there are also density-based models. However, they depend on input parameters, too, and the parameters can have a strong influence on the outcome. Based on the data from task 1, apply DBSCAN for each $eps\in \{1,5,10\}$, with $eps$ representing the epsilon threshold for density-connectivity. As the number of minimum points required in the $eps$ neighborhood of core points use $minPoints = 4$. Display the cluster assignments of the points in a scatter plot. (You may use `dbscan` from package `dbscan`.)

```{r}
# Solution of task 2...

eps <- c(1, 5, 10)
# The tilde creates a formula and the dot represents a placeholder for the (single) argument K to define the number of centers/clusters
list_clu_res_dbs <- map(eps, ~dbscan(student, eps = ., minPts =  4))

for(i in seq_along(eps)) {
  student_clu_dbs <- student %>%
    add_column(cluster = list_clu_res_dbs[[i]]$cluster) %>%
    mutate(cluster = factor(cluster))
  
  print(ggplot(student_clu_dbs, aes(Exam1, Exam2, color = cluster, fill = cluster)) +
    geom_point(pch = 21) +
    labs(title = str_c("eps=", eps[i]))
    )
}

```

3. For the clustering results from task 1 and 2, use the silhouette coefficient to find the optimal cluster parameters (i.e., for $K$-means the number of clusters $K$, and for DBSCAN the epsilon threshold for density-connectivity $eps$). (You may use `silhouette` from package `cluster`.)

```{r}
# Solution of task 3...
# For K-means
silh_k <- map_dbl(list_clu_res_k, ~mean(silhouette(.$cluster, dist(student)) %>% .[,3]))
                                                               
student_silh_k <- tibble(K = K, Silh = silh_k)

student_silh_k %>%
  ggplot(aes(K, Silh)) +
  geom_line() + 
  scale_y_continuous(limits = c(-0.2,0.6))

# For DBSCAN
silh_dbs <- map_dbl(list_clu_res_dbs, ~mean(silhouette(.$cluster, dist(student)) %>% .[,3]))
                                                               
student_silh_dbs <- tibble(eps = eps, Silh = silh_dbs)

student_silh_dbs %>%
  ggplot(aes(eps, Silh)) +
  geom_line() + 
  scale_y_continuous(limits = c(-0.2,0.6))
```


4. The following distance matrix is given. Perform agglomerative hierarchical clustering with  _single_ and _complete_ linkage. Display the result in a dendrogram. The dendrogram should represent the order in which the points are joined. (You may use `hclust` from package `cluster`/`stats`.)

```{r}
dm <- tribble(~p1,~p2,~p3,~p4,~p5,
              0.00, 0.02, 0.90, 0.36, 0.53,
              0.02, 0.00, 0.65, 0.15, 0.24,
              0.90, 0.65, 0.00, 0.59, 0.45,
              0.36, 0.15, 0.59, 0.00, 0.56,
              0.53, 0.24, 0.45, 0.56, 0.00) %>% as.matrix()
rownames(dm) <- letters[1:5]
colnames(dm) <- letters[1:5]
knitr::kable(dm)
```

```{r}
# Solution of task 4...

plot(as.dendrogram(hclust(as.dist(dm), "single")))

plot(as.dendrogram(hclust(as.dist(dm), "complete")))
```

------