# The body mass index (BMI)
# is a measure of body fat based on height and weight that applies to adult men and women.
# And The basal metabolic rate (BMR)
# is the amount of energy needed while resting in a temperate environment when the digestive system is inactive.
# To calculate them this application will ask you to enter your birth date, your gender, your weight and height using standard or metric measures.
# Then select the gender "Compute BMI and BMR" your BMI and BMR will appear below.

# Get the user's gender, birthdate, height, and weight.
fullName <- readline(prompt = "Enter your fullname: ")
name <- fullName
gender <-  readline(prompt = "Please enter your gender (M or F): ")
Gender <- toupper(gender)
birthdate <- readline(prompt = "Enter your birthdate (YYYY-MM-DD): ")
weight <- readline(prompt = "Enter your weight in U.S. pounds: ")
Weight <- as.numeric(weight)
height <- readline(prompt = "Enter your height in U.S. inches: ")
Height <- as.numeric(height)

# Create a function to compute age.
compute_age <- function(birthdate) {
    # Compute and return a person's age in years.
    # Parameter birth_str: a person's birthdate stored
    # as a string in this format: YYYY-MM-DD
    # Return: a person's age in years.

    # Convert a person's birthdate and current date; from a string
    # to a date integer.
    Birthdate <- as.Date(birthdate) 
    birth_year <- substring(Birthdate,1,4) 
    BirthYear <- as.integer(birth_year)
    currentDate <- Sys.Date()
    current_year <- substring(currentDate, 1, 4)
    CurrentYear <- as.integer(current_year)
    # Compute the difference between today and the
    # person's birthdate in years.
    age = CurrentYear - BirthYear
    # If necessary, subtract one from the difference.
    birthdate_month <- substring(Birthdate, 6, 7)
    BirthdateMonth <- as.integer(birthdate_month)
    birth_day <- substring(Birthdate, 9, 10)
    BirthDay <- as.integer(birth_day)
    current_month <- substring(currentDate, 6, 7)
    CurrentMonth <- as.integer(current_month)
    current_day <- substring(currentDate, 9, 10)
    CurrentDay <- as.integer(current_day)
    if (BirthdateMonth > CurrentMonth ||
        BirthdateMonth == CurrentMonth &&
        BirthDay > CurrentDay) {
                 age <- -1
            }
    return(age)
}
Age <- compute_age(birthdate)

kg_from_lb <- function(pounds) {
    # Convert a mass in pounds to kilograms.
    # Parameter pounds: a mass in U.S. pounds.
    # Return: the mass in kilograms.
    x <- as.numeric(pounds)
    mass = x * 0.45359237
    mass_round <- round(mass, 2);
    return(mass_round)    
}

cm_from_in <- function(inches){
    # Convert a length in inches to centimeters.
    # Parameter inches: a length in inches.
    # Return: the length in centimeters.
    x <- as.numeric(inches)
    centimeters = 2.54 * x
    centimeters_round = round(centimeters, 2);
    return(centimeters_round)
}

body_mass_index <- function(weight, height){
    # Compute and return a person's body mass index.
    # Parameters
        # weight: a person's weight in kilograms.
        # height: a person's height in centimeters.
    # Return: a person's body mass index.
    w <- as.numeric(weight)
    h <- as.numeric(height)
    bmi = w * 10000 / h ** 2
    bmi_rounding = round(bmi, 2);
    return(bmi_rounding)
}

basal_metabolic_rate <- function(gender, weight, height, age){
    # Compute and return a person's basal metabolic rate.
    # Parameters
        # weight: a person's weight in kilograms.
        # height: a person's height in centimeters.
        # age: a person's age in years.
    # Return: a person's basal metabolic rate in kcals per day.
    w <- as.numeric(weight)
    h <- as.numeric(height)
    a <- as.integer(age)
    
    if (gender == "F") {
        bmr = 447.593 + (9.247 * w) + (3.098 * h) - (4.330 * a)
    }else if (gender == "M") {
        bmr = 88.362 + (13.397 * w) + (4.799 * h) - (5.677 * a)
        
    }
    bmr_rounding = round(bmr, 2);
    return(bmr_rounding)
}

# Call my functions
kl <- kg_from_lb(Weight)
ci <- cm_from_in(height)
bmi <- body_mass_index(Weight, Height)
bmr <- basal_metabolic_rate(Gender, Weight, Height, Age)

# Create a data frame
data_frame <- data.frame(Person = c("Name", "Age", "Gender", "Weight(kg)", "Height(cm)", "Body mass index", "Basal metabolic rate"),
Characteristics = c(name, Age, Gender, kl, ci, bmi, bmr)
)

# Print the data frame
print(data_frame)
