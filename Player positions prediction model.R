library(keras)
library(ggplot2)
library(dplyr)
library(ggsoccer)
library(corrplot)
library(RColorBrewer)
library(data.table)
library(gganimate)

# Data Formatting
# loading data
tracking.away <-
        read.csv("data/Sample_Game_2/Sample_Game_2_RawTrackingData_Away_Team.csv",
                 skip = 2)
tracking.home <-
        read.csv("data/Sample_Game_2/Sample_Game_2_RawTrackingData_Home_Team.csv",
                 skip = 2)
events <-
        read.csv("data/Sample_Game_2/Sample_Game_2_RawEventsData.csv")

# Renaming columns
away.names <-
        c(
                "Period",
                "Frame",
                "Time",
                "away_25_x",
                "away_25_y",
                "away_15_x",
                "away_15_y",
                "away_16_x",
                "away_16_y",
                "away_17_x",
                "away_17_y",
                "away_18_x",
                "away_18_y",
                "away_19_x",
                "away_19_y",
                "away_20_x",
                "away_20_y",
                "away_21_x",
                "away_21_y",
                "away_22_x",
                "away_22_y",
                "away_23_x",
                "away_23_y",
                "away_24_x",
                "away_24_y",
                "away_26_x",
                "away_26_y",
                "ball_x",
                "ball_y"
        )

home.names <-
        c(
                "Period",
                "Frame",
                "Time",
                "home_11_x",
                "home_11_y",
                "home_1_x",
                "home_1_y",
                "home_2_x",
                "home_2_y",
                "home_3_x",
                "home_3_y",
                "home_4_x",
                "home_4_y",
                "home_5_x",
                "home_5_y",
                "home_6_x",
                "home_6_y",
                "home_7_x",
                "home_7_y",
                "home_8_x",
                "home_8_y",
                "home_9_x",
                "home_9_y",
                "home_10_x",
                "home_10_y",
                "home_12_x",
                "home_12_y",
                "home_13_x",
                "home_13_y",
                "home_14_x",
                "home_14_y",
                "ball_x",
                "ball_y"
        )

names(tracking.away) <- away.names
names(tracking.home) <- home.names


# Removing ball location from home team
# Removing substitutes
tracking.away <- tracking.away[, -c(26, 27)]
tracking.home <- tracking.home[, 1:25]

# Merging home and away and removing NAs
tracking <-
        merge(
                tracking.home,
                tracking.away,
                by = c("Period", "Frame", "Time"),
                all = T
        ) %>%
        arrange(Frame)  %>% na.omit()

# to single playing direction so that each team is shooting at the same goal all the match
x.colomns <- tracking %>% select(contains("_x")) %>% names()
tracking[tracking$Period == 2, x.colomns] <-
        1 -  tracking[tracking$Period == 2, x.colomns]


# transforming to metric coordinates
x.colomns <- tracking %>% select(contains("_x")) %>% names()
y.colomns <- tracking %>% select(contains("_y")) %>% names()
tracking[x.colomns] <- tracking[x.colomns] * 106
tracking[y.colomns] <- tracking[y.colomns] * 68


##### plot for all players positions at certain frame #####
# making a data frame for one frame from the match
frame.num = 121055
frame.x <-
        tracking[tracking$Frame == frame.num, ] %>% select(contains("_x")) %>% t() %>% na.omit()
frame.y <-
        tracking[tracking$Frame == frame.num, ] %>% select(contains("_y")) %>% t() %>% na.omit()
frame.color <-  c(rep("home", 11), rep("away", 11), "ball")
frame <- data.frame(frame.x, frame.y, frame.color)
names(frame) <- c("x", "y", "color")

#using tiff() and dev.off
tiff(
        'plot2.png',
        units = "in",
        width = 11.5,
        height = 8,
        res = 300,
        compression = 'lzw'
)
ggplot() +
        # Plot the pitch
        annotate_pitch(dimensions = pitch_statsbomb,
                       fill = "gray7",
                       colour = "#EBF0F2") +
        theme_pitch() + direction_label(x_label = 60) +
        
        # Plot the passes start points with different color for each cluster
        geom_point(
                data = frame[1:22, ],
                aes(x = x , y = y , color = color),
                alpha = 1,
                size = 9,
                stroke = 0,
                shape = 16
        ) +
        geom_point(
                data = frame[23, ],
                aes(x = x , y = y , color = color),
                alpha = 1,
                size = 7,
                stroke = 0,
                shape = 16
        ) +
        scale_color_manual(values = c("#ffba08", "#d00000", "#3db2d2")) +
        theme(legend.position = "none") +
        
        # titles
        labs(title = "Sample of players positioning from the data") +
        
        # customizing the plot
        theme(
                plot.title = element_text(face = "bold", color = "#D8D8D6", size = 22),
                plot.subtitle = element_text(color = "#D8D8D6", size = 16),
                plot.caption = element_text(color = "#D8D8D6", size = 12),
                plot.title.position = "panel",
                plot.background = element_rect(fill = "gray7"),
                legend.position = "top",
                legend.key = element_rect(fill = "gray7"),
                legend.background = element_rect(fill = "gray7"),
                legend.justification = "center",
                legend.text = element_text(color = "#D8D8D6"),
                legend.title = element_text(face = "bold", color = "#D8D8D6"),
                legend.box.spacing = unit(0, "pt"),
                legend.margin = margin(20)
        )

dev.off()

##### plot for one player movement between two frames #####
# making a data frame for one frame from the match

start.frame = 10000
end.frame = 30000
player.num = c("home_11", "home_3", "home_2", "home_4", "home_10")
players <- data.frame(row.names = c("x", "y", "Player"))
for (p in player.num) {
        player.temp <-
                tracking[start.frame:end.frame, ] %>% select(contains(p)) %>%
                mutate(rep(paste("Player", p), end.frame - start.frame + 1))
        names(player.temp) <- c("x", "y", "Player")
        
        players <- rbind(players, player.temp)
        
}


#using tiff() and dev.off
tiff(
        'plot5.png',
        units = "in",
        width = 11.5,
        height = 8,
        res = 300,
        compression = 'lzw'
)
ggplot() +
        # Plot the pitch
        annotate_pitch(dimensions = pitch_statsbomb,
                       fill = "gray7",
                       colour = "#EBF0F2") +
        theme_pitch() + direction_label(x_label = 60) +
        
        # Plot the passes start points with different color for each cluster
        geom_path(
                data = players,
                aes(x = x , y = y, color = Player),
                alpha = 1,
                size = 2
        ) +
        scale_color_manual(values = c("#ffba08", "#d00000", "#3db2d2", "#032b43", "#136f63")) +
        theme(legend.position = "none") +
        
        # titles multiple
        labs(title = "What is Total Football",
             subtitle = "From frame 10000 to frame 30000") +
        
        
        # customizing the plot
        theme(
                plot.title = element_text(face = "bold", color = "#D8D8D6", size = 22),
                plot.subtitle = element_text(color = "#D8D8D6", size = 16),
                plot.caption = element_text(color = "#D8D8D6", size = 12),
                plot.title.position = "panel",
                plot.background = element_rect(fill = "gray7"),
                legend.position = "top",
                legend.key = element_rect(fill = "gray7"),
                legend.background = element_rect(fill = "gray7"),
                legend.justification = "center",
                legend.text = element_text(color = "#D8D8D6"),
                legend.title = element_text(face = "bold", color = "#D8D8D6"),
                legend.box.spacing = unit(0, "pt"),
                legend.margin = margin(20)
        )

dev.off()


##### Correlation Matrix #####

tiff(
        'plot6.png',
        units = "in",
        width = 10,
        height = 10,
        res = 300,
        compression = 'lzw'
)

corrplot(
        cor(tracking[, -c(1, 2, 3)]),
        type = "upper",
        order = "hclust",
        tl.col = "black",
        col = brewer.pal(n = 8, name = "RdYlBu")
)

dev.off()


##### initial model: linear regression #####

# removing the columns about time
# Creating training and testing sets and removing the columns about time
intrain <- 1:floor(0.8 * nrow(tracking))
train <- tracking[intrain, ] %>% select(-c(1, 2, 3)) %>% na.omit()
test <- tracking[-intrain, ] %>% select(-c(1, 2, 3)) %>% na.omit()


# fitting the model for one player
fit.home.8.x <- lm(data = train, home_8_x ~ .,)
fit.home.6.x <- lm(data = train, home_6_x ~ .,)
fit.home.10.y <- lm(data = train, home_10_y ~ .,)
fit.home.7.y <- lm(data = train, home_7_y ~ .,)
fit.home.2.y <- lm(data = train, home_2_y ~ .,)

pred.home.6.x <- predict(fit.home.6.x, data = test)


tiff(
        'plot7.png',
        units = "in",
        width = 10,
        height = 10,
        res = 300,
        compression = 'lzw'
)
par(mfrow = c(2, 2))
plot(fit.home.8.x)
dev.off()

##### DNN using positions #####

player.to.train <- "home_6"

x_train <- train %>% select(-contains(player.to.train)) %>% as.matrix()
x_test <- test %>% select(-contains(player.to.train)) %>% as.matrix()

y_train <- train %>% select(contains(player.to.train)) %>% as.matrix()
y_test <- test %>% select(contains(player.to.train)) %>% as.matrix()

n_features = ncol(x_train)

model <- keras_model_sequential()
model %>%
        layer_dense(units = 80,
                    activation = 'relu',
                    input_shape = 44) %>%
        layer_dropout(0.3) %>%
        layer_dense(units = 60, activation = 'relu') %>%
        layer_dropout(0.3) %>%
        layer_dense(units = 30, activation = 'relu') %>%
        layer_dropout(0.3) %>%
        layer_dense(units = 10, activation = 'relu') %>%
        layer_dense(units = 2)

model %>% compile(
        optimizer = optimizer_adam(),
        loss = 'mae',
        metrics = c('accuracy')
)
history <-
        model %>% fit(
                x_train,
                y_train,
                epochs = 30,
                batch_size = 32,
                validation_split = 0.3
        )

tiff(
        'plot8.png',
        units = "in",
        width = 7,
        height = 7,
        res = 300,
        compression = 'lzw'
)
plot(history)
dev.off()

model %>% evaluate(x_test, y_test)

##### RNN using positions #####

player.to.train <- "home_5"

x_train <- train %>% select(-contains(player.to.train)) %>% as.matrix()
x_test <- test %>% select(-contains(player.to.train)) %>% as.matrix()

y_train <- train %>% select(contains(player.to.train)) %>% as.matrix()
y_test <- test %>% select(contains(player.to.train)) %>% as.matrix()

# function to transform multivariate time series to one data frame
time.series <- function (data) {
        
        # making first column, shifting to make the last 25 observations
        rslt <- data[, 1] %>% shift(n = 25:0, type = "lead") %>% as.data.frame()
        
        # looping over all columns
        for (i in 2:ncol(data)) {
                x.seq <- data[, i] %>% shift(n = 25:0, type = "lead") %>% as.data.frame()
                rslt <- cbind(rslt, x.seq)
                
        }
        
        # removing NA's and making 3d array
        rslt <- rslt %>% na.omit() %>% as.matrix() %>%
                array_reshape(dim = c(nrow(data) - 25, 26, ncol(data)))
        
}

# no comment for this, its obvious!
x_train_seq <- time.series(x_train)
x_test_seq <- time.series(x_test)

y_train_seq <- y_train[-(1:25),]
y_test_seq <- y_test[-(1:25),]


model <- keras_model_sequential()

model %>% layer_lstm(100, input_shape = c(26, 44)) %>%
        layer_dense(2)

model %>% compile(
        optimizer = optimizer_adam(),
        loss = 'mae',
        metrics = c('accuracy')
)
history <-
        model %>% fit(
                x_train_seq,
                y_train_seq,
                epochs = 15,
                batch_size = 512,
                validation_split = 0.3)

tiff(
        'plot10.png',
        units = "in",
        width = 7,
        height = 7,
        res = 300,
        compression = 'lzw'
)
plot(history)
dev.off()

model %>% evaluate(x_test_seq, y_test_seq)


#### DNN with velocities 
players <- gsub('.{2}$', '', names(train))
dt <- 0.04
for (player in players) {
        vx <- smooth(diff(tracking[,paste(player, "_x", sep = "")]) / dt)
        vy <- smooth(diff(tracking[,paste(player, "_y", sep = "")]) / dt)
        
        tracking[paste(player, "_vx", sep = "")] <- c(NA,vx)
        tracking[paste(player, "_vy", sep = "")] <- c(NA,vy)
        
}

tracking <- tracking %>% na.omit()

# Splitting data into training and testing
intrain <- 1:floor(0.8 * nrow(tracking))
train <- tracking[intrain, ] %>% select(-c(1, 2, 3)) %>% na.omit()
test <- tracking[-intrain, ] %>% select(-c(1, 2, 3)) %>% na.omit()

players.away <- c("away_25", "away_15", "away_16",
                  "away_17", "away_18", "away_19", "away_20", "away_21", "away_22",
                  "away_23", "away_24")
# Splitting data into input and output
models <- list()
acc <- list()
for (player.to.train in players.away) {
        
        varx <- paste(player.to.train, "_x", sep = "")
        vary <- paste(player.to.train, "_y", sep = "")
        
        x_train <- train %>% select(-contains(player.to.train)) %>% as.matrix()
        x_test <- test %>% select(-contains(player.to.train)) %>% as.matrix()
        
        y_train <- train %>% select(varx, vary) %>% as.matrix()
        y_test <- test %>% select(varx, vary) %>% as.matrix()
        
        
        model <- keras_model_sequential()
        model %>%
                layer_dense(units = 200,
                            activation = 'relu',
                            input_shape = 88) %>%
                layer_dense(units = 120, activation = 'relu',
                            kernel_regularizer = regularizer_l1_l2(l1 = 0.1, l2 = 0.01)) %>%
                layer_dense(units = 60, activation = 'relu',
                            kernel_regularizer = regularizer_l1_l2(l1 = 0.1, l2 = 0.01)) %>%
                layer_dense(units = 30, activation = 'relu',
                            kernel_regularizer = regularizer_l1_l2(l1 = 0.1, l2 = 0.01)) %>%
                layer_dense(units = 10, activation = 'relu',
                            kernel_regularizer = regularizer_l1_l2(l1 = 0.1, l2 = 0.01)) %>%
                layer_dense(units = 2)
        
        model %>% compile(
                optimizer = optimizer_adam(),
                loss = 'mae',
                metrics = c('accuracy')
        )
        history <-
                model %>% fit(
                        x_train,
                        y_train,
                        epochs = 11,
                        batch_size = 32,
                        validation_split = 0.3
                )
        
        
        acc <- append(acc, (model %>% evaluate(x_test, y_test)))
        
        # making predictions
        y_pred <- predict(model, x_test) %>% as.data.frame() 
        y_test <- as.data.frame(y_test)
        
        # Unifying names
        names(y_pred) <- names(y_test)
        plot.df <- rbind(y_pred[1:5000,], y_test[1:5000,]) %>% 
                cbind(Data = rep(c("Predected", "Actual"), each = 5000))
        
        # plotting the predictions vs actual movements
        tiff(
                paste(player.to.train, ".png", sep = ""),
                units = "in",
                width = 11.5,
                height = 8,
                res = 300,
                compression = 'lzw'
        )
        ggplot() +
                # Plot the pitch
                annotate_pitch(dimensions = pitch_statsbomb,
                               fill = "gray7",
                               colour = "#EBF0F2") +
                theme_pitch() + direction_label(x_label = 60) +
                
                # Plot the passes start points with different color for each cluster
                geom_path(
                        data = plot.df,
                        aes(x = away_19_x , y = away_19_y, color = Data),
                        alpha = 1,
                        size = 2
                ) +
                scale_color_manual(values = c("#ffba08", "#3db2d2")) +
                theme(legend.position = "none") +
                
                # titles multiple
                labs(title = "The actual movement VS predicted movement on test set",
                     subtitle = "5000 fram on player away 19") +
                
                
                # customizing the plot
                theme(
                        plot.title = element_text(face = "bold", color = "#D8D8D6", size = 22),
                        plot.subtitle = element_text(color = "#D8D8D6", size = 16),
                        plot.caption = element_text(color = "#D8D8D6", size = 12),
                        plot.title.position = "panel",
                        plot.background = element_rect(fill = "gray7"),
                        legend.position = "top",
                        legend.key = element_rect(fill = "gray7"),
                        legend.background = element_rect(fill = "gray7"),
                        legend.justification = "center",
                        legend.text = element_text(color = "#D8D8D6"),
                        legend.title = element_text(face = "bold", color = "#D8D8D6"),
                        legend.box.spacing = unit(0, "pt"),
                        legend.margin = margin(20)
                )
        
        dev.off()
        
        models <- append(models, model)
        
}

# saving models
for (i in 1:11) {
        
        save_model_tf(models[[i]], paste("model_", i, "/", sep = ""))
}

# reading models
models <- list()
for (i in 1:11) {
        models <- append(models, load_model_tf(paste("model_", i, "/", sep = "")))
}

names(models) <- players.away

# ###### Animation ######
x_train <- list()
x_test <- list()

y_train <- list()
y_test <- list()

for (player.to.train in players.away) {
        
        varx <- paste(player.to.train, "_x", sep = "")
        vary <- paste(player.to.train, "_y", sep = "")
        
        x_train <- append(x_train, (train %>% select(-contains(player.to.train))))
        x_test <- append(x_test, (test %>% select(-contains(player.to.train))))
        
        y_train <- append(y_train, (train %>% select(varx, vary)))
        y_test <- append(y_test, (test %>% select(varx, vary)))
        
}

##### plot for all players positions at certain frame #####
# making a data frame for one frame from the match
frame <- data.frame(x = NA, y = NA, color = NA, frame.id = NA)
for (frame.num in 5000:5100) {
        
        frame.x <- tracking[tracking$Frame == frame.num, ] %>% select(contains("_x")) %>% t() %>% na.omit()
        frame.y <-tracking[tracking$Frame == frame.num, ] %>% select(contains("_y")) %>% t() %>% na.omit()
        frame.color <-  c(rep("home", 11), rep("away", 11), "ball")
        frame.id <- rep(frame.num, 23)
        
        frame.temp <- data.frame(frame.x, frame.y, frame.color, frame.id)
        names(frame.temp) <- c("x", "y", "color", "frame.id")
        frame <- rbind(frame, frame.temp)
        
}

frame <- frame[-1,]


animation <- ggplot() +
        # Plot the pitch
        annotate_pitch(dimensions = pitch_statsbomb,
                       fill = "gray7",
                       colour = "#EBF0F2") +
        theme_pitch() + direction_label(x_label = 60) +
        
        # Plot the passes start points with different color for each cluster
        geom_point(
                data = frame,
                aes(x = x , y = y , color = color),
                alpha = 1,
                size = 9,
                stroke = 0,
                shape = 16
        ) +
 
        scale_color_manual(values = c("#ffba08", "#d00000", "#3db2d2")) +
        theme(legend.position = "none") +
        
        # titles
        labs(title = "Sample of players positioning from the data") +
        
        # customizing the plot
        theme(
                plot.title = element_text(face = "bold", color = "#D8D8D6", size = 22),
                plot.subtitle = element_text(color = "#D8D8D6", size = 16),
                plot.caption = element_text(color = "#D8D8D6", size = 12),
                plot.title.position = "panel",
                plot.background = element_rect(fill = "gray7"),
                legend.position = "top",
                legend.key = element_rect(fill = "gray7"),
                legend.background = element_rect(fill = "gray7"),
                legend.justification = "center",
                legend.text = element_text(color = "#D8D8D6"),
                legend.title = element_text(face = "bold", color = "#D8D8D6"),
                legend.box.spacing = unit(0, "pt"),
                legend.margin = margin(20)
        ) +
        
        # gganimate code
        ggtitle("{frame}") +
        transition_time(frame.id) +
        ease_aes("linear") +
        enter_fade() +
        exit_fade()

animate(animation)

animate(animation, renderer = ffmpeg_renderer(),fps = 20, width = 1150, height = 800)
anim_save("animation.mp4")
