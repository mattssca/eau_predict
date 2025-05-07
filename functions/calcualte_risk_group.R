#' @title Calculate Risk Group Based on Metadata
#'
#' @description This function calculates the risk group for each sample in a metadata data frame based on 
#' specified weights and methods.
#' 
#' @details The calculation of the risk group is based on the paper by Richard J. Sylvester et al. 
#' (2021) titled: "European Association of Urology (EAU) Prognostic Factor Risk Groups for 
#' Non–muscle-invasive Bladder Cancer (NMIBC) Incorporating the WHO 2004/2016 and WHO 1973 
#' Classification Systems for Grade: An Update from the EAU NMIBC Guidelines Panel". The function 
#' uses two sets of weights and risk group breaks corresponding to the WHO 2004/2016 and WHO 1973 
#' classification systems.
#'
#' @param this_metadata A data frame containing the metadata for the samples.
#' @param age_col The name of the column in the data frame that contains the age information.
#' @param tumors_col The name of the column in the data frame that contains the number of tumors information.
#' @param diameter_col The name of the column in the data frame that contains the maximum diameter information.
#' @param stage_col The name of the column in the data frame that contains the stage information.
#' @param cis_col The name of the column in the data frame that contains the concomitant CIS information.
#' @param grade_col The name of the column in the data frame that contains the grade information.
#' @param method The method to use for calculating the risk group. Can be either "who_2016" or "who_1973". Default is "who_2016".
#'
#' @return A data frame with the original metadata and an additional column "risk_group" indicating 
#' the calculated risk group.
#'
#' @export
#'
#' @examples
#' #sample metadata data frame
#' 
#' metadata <- data.frame(
#'  SampleID = 1:5,
#'  Age = c(56, 75, 45, 15, 80),
#'  Number_of_Tumors = c(1, 1, 3, 6, 20),
#'  Maximun_Diameter = c(1, 3, 4, 2, 5),
#'  Stage = c("Ta", "T1", "Ta", "T1", "T1"),
#'  Concomitant_CIS = c("No", "Yes", "No", "Yes", "Yes"),
#'  WHO_Grade = c("LG", "HG", "LG", "HG", "HG"),
#'  WHO_1974_Grade = c("G1", "G2", "G1", "G3", "G3"))
#'
#' #run the function
#' result_method1 <- calculate_risk_group(this_metadata = metadata, 
#'                                        age_col = "Age", 
#'                                        tumors_col = "Number_of_Tumors", 
#'                                        diameter_col = "Maximun_Diameter", 
#'                                        stage_col = "Stage", 
#'                                        cis_col = "Concomitant_CIS", 
#'                                        grade_col = "WHO_Grade", 
#'                                        method = "who_2016")
#' 
calculate_risk_group = function(this_metadata, 
                                age_col, 
                                tumors_col, 
                                diameter_col, 
                                stage_col, 
                                cis_col, 
                                grade_col, 
                                method = "who_2016"){
  
  #define the weights for WHO 2016
  weights_who_2016 = list(
    "Age: ≤70 yr" = 0,
    "Age: >70 yr" = 55,
    "Number of Tumors: single" = 0,
    "Number of Tumors: multiple" = 50,
    "Maximun Diameter: <3 cm" = 0,
    "Maximun Diameter: ≥3 cm" = 65,
    "Stage: Ta" = 0,
    "Stage: T1" = 80,
    "Concomitant CIS: No" = 0,
    "Concomitant CIS: Yes" = 100,
    "WHO 2004 grade: LG" = 0,
    "WHO 2004 grade: HG" = 85
  )
  
  #define the weights for WHO 1973
  weights_who_1973 = list(
    "Age: ≤70 yr" = 0,
    "Age: >70 yr" = 32,
    "Number of Tumors: single" = 0,
    "Number of Tumors: multiple" = 32,
    "Maximun Diameter: <3 cm" = 0,
    "Maximun Diameter: ≥3 cm" = 43,
    "Stage: Ta" = 0,
    "Stage: T1" = 52,
    "Concomitant CIS: No" = 0,
    "Concomitant CIS: Yes" = 58,
    "WHO 1974 grade: G1" = 0,
    "WHO 1974 grade: G2" = 58,
    "WHO 1974 grade: G3" = 100
  )
  
  #check that the requested columns exist in the data frame
  required_cols = c(age_col, tumors_col, diameter_col, stage_col, cis_col, grade_col)
  
  
  if(!all(required_cols %in% colnames(this_metadata))) {
    stop("One or more specified columns do not exist in the incoming metadata...")
  }
  
  #select the appropriate weights and risk group breaks based on the method
  if(method == "who_2016"){
    weights <- weights_who_2016
    risk_breaks <- c(-Inf, 80, 150, 305, Inf)
    risk_labels <- c("Low risk", "Intermediate risk", "High risk", "Very high risk")
  }else if (method == "who_1973"){
    weights <- weights_who_1973
    risk_breaks <- c(-Inf, 52, 133, 233, Inf)
    risk_labels <- c("Low risk", "Intermediate risk", "High risk", "Very high risk")
  }else{
    stop("Invalid method specified. Choose either 'who_2016' or 'who_1973'.")
  }
  
  #calculate the total weighted score for each sample
  this_metadata$total_score <- 0
  this_metadata$total_score <- this_metadata$total_score + ifelse(this_metadata[[age_col]] <= 70, weights[["Age: ≤70 yr"]], weights[["Age: >70 yr"]])
  this_metadata$total_score <- this_metadata$total_score + ifelse(this_metadata[[tumors_col]] == 1, weights[["Number of Tumors: single"]], weights[["Number of Tumors: multiple"]])
  this_metadata$total_score <- this_metadata$total_score + ifelse(this_metadata[[diameter_col]] < 3, weights[["Maximun Diameter: <3 cm"]], weights[["Maximun Diameter: ≥3 cm"]])
  this_metadata$total_score <- this_metadata$total_score + ifelse(this_metadata[[stage_col]] == "Ta", weights[["Stage: Ta"]], weights[["Stage: T1"]])
  this_metadata$total_score <- this_metadata$total_score + ifelse(this_metadata[[cis_col]] == "No", weights[["Concomitant CIS: No"]], weights[["Concomitant CIS: Yes"]])
  
  if(method == "who_2016"){
    this_metadata$total_score <- this_metadata$total_score + ifelse(this_metadata[[grade_col]] == "LG", weights[["WHO 2004 grade: LG"]], weights[["WHO 2004 grade: HG"]])
  }else if(method == "who_1973"){
    this_metadata$total_score <- this_metadata$total_score + ifelse(this_metadata[[grade_col]] == "G1", weights[["WHO 1974 grade: G1"]], 
                                                          ifelse(this_metadata[[grade_col]] == "G2", weights[["WHO 1974 grade: G2"]], weights[["WHO 1974 grade: G3"]]))
  }
  
  #determine the risk group based on the total score
  this_metadata$risk_group <- cut(this_metadata$total_score,
                                  breaks = risk_breaks,
                                  labels = risk_labels)
  
  #return the updated data frame
  return(this_metadata)
}
