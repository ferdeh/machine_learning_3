# Customer Segementation 
## Metode yang digunakan dalam melakukan Segmentasi Pelanggan adalah dengan metode RFM Analysis
## Tujuan dari Analysis ini adalah :
## Mengenali siapa customer yang paling berharga 
## Mempelajari behavior dari pelangan
## Meningkatkan Retensi pelanggan dan revenue perusahaan


# Import Library

library(dplyr)
library(plotly)

# Data Preparation
data<-read.csv('https://raw.githubusercontent.com/arikunco/machinelearning/master/dataset/online_retail_clean.csv')


print(summary(data))
head(data)

data<-filter(data, monetary >= 0) %>% na.omit(data)
print(summary(data))

### Data sudah terformat rapih dan disap untuk dilakukan langkah selanjutnya


# RFM Scoring

### Metode RFM Scoring adalah metode dengan membagi setiap variable RFM menjadi 4 bagian dan memberikan score pada masing-masing bagian

### Encode dataset breakdown into 4 for each variables 
r <- quantile(data$recency,c(0.25,0.5,0.75))
f <- quantile(data$frequency, c(0.25,0.5,0.75))
m <- quantile(data$monetary, c(0.25,0.5,0.75))

### membagi data frame menjadi empat group berdasarkan nilai quartile nya
df_encode <- data %>% mutate(m_recency=ifelse(recency<as.numeric(r[1]),4,
                                              ifelse(recency<as.numeric(r[2]),3,
                                                     ifelse(recency<as.numeric(r[3]),2,1))),
                             m_frequency=ifelse(frequency<as.numeric(f[1]),1,
                                                ifelse(frequency<as.numeric(f[2]),2,
                                                       ifelse(frequency<as.numeric(f[3]),3,4))),
                             m_monetary=ifelse(monetary<as.numeric(m[1]),1,
                                               ifelse(monetary<as.numeric(m[2]),2,
                                                      ifelse(monetary<as.numeric(m[3]),3,4))))

### Group 1 adalah group yang kurang baik dan group 4 adalah group terbaik
### Untuk recency Quartile 1 adalah group 4 karena semakin kecil nilai rencency maka dianggap semakin baik
### Untuk Frequency dan MOnetary Quartile 1 adalah group 1 karena semakin jarang melakukan pembelian dan semakin kecil nilai pembelian dianggap semakin kurang baik


### preview first 6th rows
head(df_encode)

### menghitung RFM Score 
df_encode$rfmscore<-(df_encode$m_recency*100)+(df_encode$m_frequency*10)+(df_encode$m_monetary)

head(df_encode)
str(df_encode)
## Customer Segment by RFM Score
nrow(filter(df_encode,rfmscore == 444))

print(paste("Best Customer :", nrow(filter(df_encode,rfmscore == 444))))
print(paste("Loyal Customer :", nrow(filter(df_encode,m_recency == 4))))
print(paste("Big Spender Customer :", nrow(filter(df_encode,m_monetary == 4))))
print(paste("Lost Customer :", nrow(filter(df_encode,rfmscore == 144))))
print(paste("Lost Cheap Customer :", nrow(filter(df_encode,rfmscore == 111))))

plot(df_encode[,2:4], col = df_encode$rfmscore,
     main = "Group by RFM Score")

#### 3D plot
p <- plot_ly(df_encode, x = ~recency, y = ~frequency, z = ~monetary, color = ~rfmscore) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Recency'),
                      yaxis = list(title = 'Frequency'),
                      zaxis = list(title = 'Monetary')))

p


# RFM Model menggunakan Kmean





# Select encoded data
df_cluster <- df_encode %>% select (CustomerID,m_recency,m_frequency,m_monetary)

# Mengeksplorasi struktur dataset cars
head(df_cluster)
str(df_cluster)
summary(df_cluster)

# Normalized data (terkadang perlu dinormalisasi apabila interval datanya sangat besar)
# df_norm <- as.data.frame(apply(df_cluster[, 2:4], 2, function(x) (x - min(x))/(max(x)-min(x))))

# As it is (tanpa normalisasi, hilangkan customerID)
df_norm <- df_cluster[,c(2:4)]

# Menentukan nilai K (Determine Number of Clusters)
# Initialize total within sum of squares error: wss
wss <- 0

# For 1 to 15 cluster centers
for (i in 1:15) {
  km.out <- kmeans(df_norm, centers = i, nstart = 20)
  # Save total within sum of squares to wss variable
  wss[i] <- km.out$tot.withinss
}

# Plot total within sum of squares vs. number of clusters
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")


# Mengelompokkan dataset ke dalam empat kategori
km <- kmeans(df_norm, 4)

# Memperlihatkan isi dari cluster
km

# Memperlihatkan titik sentroid cluster
km$centers

# Memperlihatkan cluster
km$cluster

# Inspect the result
summary(km)

# Scatter plot of x
pairs(x[,c(2:4)], col = km$cluster, pch=20)
head(x)
# final 
final <- data.frame(x,cluster=km$cluster)
head(final)
# visualisasi boxplot untuk masing-masing rfm
ggplot(final) + geom_boxplot(aes(x=as.factor(cluster),y=recency)) + xlab("cluster")
ggplot(final) + geom_boxplot(aes(x=as.factor(cluster),y=frequency)) + xlab("cluster")
ggplot(final) + geom_boxplot(aes(x=as.factor(cluster),y=monetary)) + xlab("cluster")

# visualisasi histogram untuk recency
ggplot(filter(final,cluster==4)) + geom_histogram(aes(x=recency), bins=10)

plot(x[,5:7], col = x$clusterhc,
     main = "HC of RFM")
