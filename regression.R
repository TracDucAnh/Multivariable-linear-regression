# Goi cac thu vien can thiet
library(dplyr)
library(tidyr)
library(caret) #Chia du lieu
library(Metrics) #Tinh toan do chinh xac mo hinh
library(pheatmap)#Ve heatmap

# Doc du lieu da duoc tien xu ly
data <- read.csv("New_GPU_Data.csv")

# Liet ke tat ca ca dac trung
colnames(data)

# Lua chon cac dac trung cho mo hinh hoi quy tuyen tinh
data_variables<-data[,c("Dedicated", "Integrated", "Manufacturer" ,"L2_Cache", "Memory_Bandwidth", "Memory_Bus",  "Open_GL", "Shader", "Memory_Type", "Notebook_GPU", "SLI_Crossfire", "Resolution_WxH",  "Memory_Speed")]

#In ra man hinh du lieu da duoc chon loc
head(data_variables)

# Chia tach cot Resolution_Width
data_variables<- data_variables %>% separate(Resolution_WxH, c('Resolution_Width', 'Resolution_Height'), sep = "x")

data_variables$Resolution_Width <- as.numeric(data_variables$Resolution_Width)

data_variables$Resolution_Height <- as.numeric(data_variables$Resolution_Height)

# Label encode cho bien Dedicated
unique(data_variables$Dedicated)
data_variables$Dedicated = factor(data_variables$Dedicated, levels = c("Yes", "No"),
                                  label = c(1, 0))
data_variables$Dedicated <- as.numeric(as.character(data_variables$Dedicated))

# Label encode cho bien Manufacturer
unique(data_variables$Manufacturer)
data_variables$Manufacturer = factor(data_variables$Manufacturer, level = c("Nvidia", "AMD", "Intel", "ATI"),
                                     label = c(0, 1, 2, 3))
data_variables$Manufacturer <- as.numeric(as.character(data_variables$Manufacturer))

# Label encode cho bien Integrated
unique(data_variables$Integrated)
data_variables$Integrated = factor(data_variables$Integrated, levels = c("Yes", "No"),
                                  label = c(1, 0))
data_variables$Integrated <- as.numeric(as.character(data_variables$Integrated))

#Label encode cho bien Notebook_GPU
unique(data_variables$Notebook_GPU)
data_variables$Notebook_GPU = factor(data_variables$Notebook_GPU, levels = c("Yes", "No"),
                                  label = c(1, 0))
data_variables$Notebook_GPU <- as.numeric(as.character(data_variables$Notebook_GPU))

#Label encode cho bien SLI_Crossfire
unique(data_variables$SLI_Crossfire)
data_variables$SLI_Crossfire = factor(data_variables$SLI_Crossfire, levels = c("Yes", "No"),
                                      label = c(1, 0))
data_variables$SLI_Crossfire <- as.numeric(as.character(data_variables$SLI_Crossfire))

#Label encode cho bien Memory_Type
unique(data_variables$Memory_Type)
data_variables$Memory_Type = factor(data_variables$Memory_Type, levels = c("GDDR3",  "GDDR4",  "GDDR5",  "DDR",    "DDR3",   "DDR4",   "GDDR5X", "HBM-2",  "DDR2", "eDRAM",  "HBM-1",  "GDDR2" ),
                                    label = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
data_variables$Memory_Type <- as.numeric(as.character(data_variables$Memory_Type))

# In du lieu da duoc xu ly
head(data_variables)

# Ma tran tuong quan
cor_matrix <- cor(data_variables)
# Ve bieu do nhiet
pheatmap(cor_matrix, 
         display_numbers = TRUE,
         color = colorRampPalette(c("blue", "white", "red"))(50),
         fontsize = 12)

# Phan chia du lieu thanh tap huan luyen va tap kiem thu
splitIndex <- createDataPartition(data_variables$Memory_Speed, p = 0.8, list = FALSE)
train_data <- data_variables[splitIndex, ]
test_data <- data_variables[-splitIndex, ]

head(train_data)

# Xay dung model1
model1 <- lm(Memory_Speed~Dedicated + Integrated + Manufacturer + L2_Cache +
             Memory_Bandwidth + Memory_Bus + Open_GL + Shader + Memory_Type + 
             Notebook_GPU + SLI_Crossfire + Resolution_Width + Resolution_Height, train_data)
# Tom tat model1
summary(model1)

#Xay dung model2
model2 <- lm(Memory_Speed~ Manufacturer + L2_Cache + Memory_Bandwidth + 
             Memory_Bus + Open_GL + Memory_Type + Notebook_GPU + SLI_Crossfire + 
             Resolution_Width, train_data)
# Tom tat model2
summary(model2)

# Kiem tra phan du
par(mfrow = c(2, 2))  # Chia bố cục đồ thị thành 2x2
plot(model2)
par(mfrow = c(1, 1))

# Kiem tra da cong tuyen
library(car)
vif(model2)

# Kiem tra phan phoi phan du
hist(residuals(model), breaks = 50, main = "Histogram of Residuals",
     xlab = "Residuals", col = "steelblue")

#Thuc hien du doan
test_data$Memory_Speed_Prediction <- predict(model2, test_data)

actual <- test_data$Memory_Speed

predicted <- test_data$Memory_Speed_Prediction

# Ve bieu do so sanh gia tri du doan voi thuc te
par(mfrow = c(1, 1)) 
plot(actual, type = "l", col = "blue", lwd = 2, 
    xlab = "Index", ylab = "Giá trị", 
     main = "Dự đoán vs Thực tế", ylim = range(c(actual, predicted)))
lines(predicted, col = "red", lwd = 2)
legend("topright", legend = c("Thực tế", "Dự đoán"), 
       col = c("blue", "red"), lty = 1, lwd = 2)

# Tinh rmse danh gia mo hinh
print(rmse(actual, predicted))

