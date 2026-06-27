total_patients <- function(df){
    nrow(df)
}
total_ckd <- function(df){
    sum(df$CKD_Status)
}
total_dialysis <- function(df){
    sum(df$Dialysis_Needed)
}
filter_data <- function(df, age, diabetes, hypertension){

    df <- df[df$Age >= age[1] &
             df$Age <= age[2], ]

    if(diabetes == "Yes")
        df <- df[df$Diabetes == 1, ]

    if(diabetes == "No")
        df <- df[df$Diabetes == 0, ]

    if(hypertension == "Yes")
        df <- df[df$Hypertension == 1, ]

    if(hypertension == "No")
        df <- df[df$Hypertension == 0, ]

    df
}