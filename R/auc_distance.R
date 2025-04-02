#' Calculate the AUC Distance between Two DataFrames
#'
#' This function calculates the Area Under the Curve (AUC) for a classification model trained on two datasets (df1 and df2).
#' It combines both datasets, assigns a class label to each, and then trains a Naive Bayes classifier to distinguish between
#' the two datasets. The AUC is computed by evaluating the model's performance on a random sample from the combined data.
#'
#' @param df1 A data frame representing the first dataset.
#' @param df2 A data frame representing the second dataset.
#'
#' @return A numeric value representing the AUC (Area Under the ROC Curve) for the classification model.
#'
#' @details
#' The function combines df1 and df2 by adding a new column `class` with values `df1` for the first dataset
#' and `df2` for the second dataset. A Naive Bayes model is trained on the combined data to predict the `class` column.
#' A random sample of 1000 observations is then selected from the combined data and used to calculate the AUC score
#' based on the predicted probabilities.
#'
#' The AUC value represents the classifier's ability to distinguish between the two datasets, where:
#' - A higher AUC value (closer to 1) indicates better performance.
#' - An AUC value of 0.5 indicates no discriminatory power (equivalent to random guessing).
#' @import dplyr
#' @importFrom e1071 naiveBayes
#' @importFrom pROC roc auc
#' @keywords internal
auc_distance<-function(df1,df2){

    df1<-df1[complete.cases(df1),]
    df2<-df2[complete.cases(df2),]
    X <-as.matrix(bind_rows(
      df1,
      df2
    ))
    y <- c(rep(0,NROW(df1)),rep(1,NROW(df2)))
    #cat('\n', sum(is.na(X)))

    #glm_model <- glmnet::glmnet(X, y, family = "binomial", alpha = 0)
    #nb_model <- e1071::naiveBayes(class ~ ., data = data)
    glm_model <- glm(y ~ X, family = "binomial")
    #pred_probs <- predict(nb_model,newdata = data, type = "raw")[,2]
    pred_probs <- predict(glm_model, newx = X, type = "response")

    # Compute the AUC
    #roc_curve <- pROC::roc(data$class, pred_probs)
    roc_curve <- pROC::roc(y, pred_probs)
    auc_value <- pROC::auc(roc_curve)
    return(auc_value)

}
