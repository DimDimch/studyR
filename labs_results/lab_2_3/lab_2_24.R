df <- read.csv('data/Payment_and_Value_of_Care-Hospital.csv')

emergency_df <- df[
  ((df$Payment.Measure.Name == 'Payment for heart attack patients') |
  (df$Payment.Measure.Name == 'Payment for heart failure patients')) , ]

hospitals <- unique(subset(df, select = c(
    'State', 
    'County.Name', 
    'City', 
    'Facility.Name'
    )
  ))

hospitals_emergency <- subset(emergency_df, select = c(
      'State', 
      'County.Name', 
      'City', 
      'Payment.Footnote'
    )
  )


fun <- function(x) {
  if (x == '') {
    1
  } else {
    0
  }
}

hospitals_emergency$Payment.Footnote <- as.integer(lapply(hospitals_emergency$Payment.Footnote, fun))


hospitals <- aggregate(hospitals$Facility.Name, 
               by = list(
                  State = hospitals$State,
                  County = hospitals$County.Name,
                  City = hospitals$City), 
               FUN = length)

hospitals_emergency <- aggregate(hospitals_emergency$Payment.Footnote, 
                       by = list(
                         State = hospitals_emergency$State,
                         County = hospitals_emergency$County.Name,
                         City = hospitals_emergency$City), 
                       FUN = function(x) floor(sum(x) / 2 + 0.5))

result <- cbind(hospitals, hospitals_emergency$x)


colnames(result) <- c('State', 'Country', 'City', 'Hospitals Total', 'Hospitals with Emergency Services')


d <- df[df$City == 'ABERDEEN', ]
# ABERDEEN AKRON

