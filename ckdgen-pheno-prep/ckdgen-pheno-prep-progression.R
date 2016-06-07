###
###		source("ckdgen-pheno-prep-progression.R")
###
###		author: Mathias Gorski
###		mathias.gorski@ukr.de
###		date: June 2nd 2016
###
###		FUNCTIONS
###		calc_CKDi25: returns a vector of length N. Calculates the binary variable CKDi25 1 = cases; 0 = controls 
###		calc_eGFRdecline: returns a vector of length N. Calculates the continuous variable eGFRdecline as decline per annum 
###		calc_rapid3: returns a vector of length N. Calculates the binary variable rapid3; 1 = cases; 0 = controls 
### 
###		INPUTs 
###		eGFRcrea_baseline: baseline eGFRcrea; vector of length N
###		eGFRcrea_followup: followup eGFRcrea; vector of length N
###		time_between_baseline_and_followup_in_years; time between baseline and followup measurement of eGFRcrea; vector of length N
###	
###		NOTE: all three input vectors must of same length
###
###		OUTPUT 
###		Vectors CKDI25, eGFRdecline and rapid3 are of length N 
###
###		EXAMPLE
###		tblPhenotypes=read.table(phenotypes.txt)  
###		names(tblPhenotypes)
###		
###		[1] "eGFRcrea_CKDEPI_baseline"	"eGFRcrea_CKDEPI_followup"	"age_baseline"	"age_followup"
###		
###		attach(tblPhenotypes)
###		timeDiff=age_followup-age_baseline
###		
###		check.decline.variables(eGFRcrea_CKDEPI_baseline,eGFRcrea_CKDEPI_followup,timeDiff)
###		CKDi25=calc_CKDi25(eGFRcrea_CKDEPI_baseline,eGFRcrea_CKDEPI_followup)
###		eGFRdecline=calc_eGFRdecline(eGFRcrea_CKDEPI_baseline,eGFRcrea_CKDEPI_followup,timeDiff)
###		rapid3=calc_rapid3(eGFRcrea_CKDEPI_baseline,eGFRcrea_CKDEPI_followup,timeDiff)
###

# check the input vectors
check.decline.variables=function(a_eGFRcrea_baseline, a_eGFRcrea_followup, a_time_between_baseline_and_followup_in_years){

	l_crea_baseline=length(a_eGFRcrea_baseline)
	l_crea_followup=length(a_eGFRcrea_followup)
	l_crea_time_diff=length(a_time_between_baseline_and_followup_in_years)

	# very simple check, if all vectors are of same length
	if (l_crea_baseline != l_crea_followup | l_crea_followup != l_crea_time_diff)
		stop(paste(date(),"Exit in check.decline.variables: input variables eGFRcrea_baseline, eGFRcrea_followup and time_between_baseline_and_followup_in_years must have same length!"))
}

# calculate dichotomous CKDi25: 1=cases; 0=controls
calc_CKDi25 = function(a_eGFRcrea_baseline, a_eGFRcrea_followup) {

	CKDi25=rep(NA,length(a_eGFRcrea_baseline))
	CKDi25[which(a_eGFRcrea_baseline >= 60 & a_eGFRcrea_followup <= (0.75*a_eGFRcrea_baseline))] = 1
	CKDi25[which(a_eGFRcrea_baseline >= 60 & a_eGFRcrea_followup >=60)] = 0
	CKDi25
}

# calculate continuous eGFRdecline
# eGFRdecline < 0 value means an increasing filtration rate per year.
# eGFRdecline > 0 of decline means a loss of filtration per year
calc_eGFRdecline = function(a_eGFRcrea_baseline, a_eGFRcrea_followup, a_time_between_baseline_and_followup_in_years) {

	eGFRdecline=(a_eGFRcrea_baseline-a_eGFRcrea_followup)/(a_time_between_baseline_and_followup_in_years)
	eGFRdecline
}

# calculate dichotomous rapid3: 1=cases; 0=controls
# rapid3==1 means more than 3 unit decline between baseline and follow up (decline of filtration rate)
# rapid3==0 means the decline between baseline and follow lies between -1 and 1 (small decline or small increase of filtration rate)
calc_rapid3 = function(a_eGFRcrea_baseline, a_eGFRcrea_followup, a_time_between_baseline_and_followup_in_years) {

	eGFRdecline=(a_eGFRcrea_baseline-a_eGFRcrea_followup)/(a_time_between_baseline_and_followup_in_years)
	rapid3=rep(NA,length(a_eGFRcrea_baseline))
	rapid3[which(eGFRdecline >= 3)] = 1
	rapid3[which(eGFRdecline > -1 & eGFRdecline < 1)] = 0
	rapid3
}
# eof