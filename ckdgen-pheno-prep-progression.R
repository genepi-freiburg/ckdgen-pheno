###
###		source("ckdgen-pheno-prep-progression.R")
###
### 	author: Mathias Gorski
###		mathias.gorski@ukr.de
###		date: April 15th 2016
###
###		FUNCTIONS
###		calc_CKDi: returns a vector of length N. Calculates the binary variable CKDi 1 = cases; 2 = controls 
###		calc_CKDi25: returns a vector of length N. Calculates the binary variable CKDi25 1 = cases; 2 = controls 
###		calc_eGFRdecline: returns a vector of length N. Calculates the continuous variable eGFRdecline as decline per annum 
###		calc_rapid3: returns a vector of length N. Calculates the binary variable rapid3; 1 = cases; 2 = controls 
### 
###		INPUT for all functions
###		eGFRcrea_baseline: baseline eGFRcrea; vector of length N
###		eGFRcrea_followup: followup eGFRcrea; vector of length N
###		time_between_baseline_and_followup_in_years; time between baseline and followup measurement of eGFRcrea; vector of length N
###	
###		NOTE: all three input vectors must of same length
###
###		OUTPUT 
###		Vectors CKDi, CKDI25, eGFRdecline and rapid3 are of length N 
###
###		EXAMPLE
###		tblPhenotypes=readm.table(phenotypes.txt)  
###		names(tblPhenotypes)
###		
###		[1] "eGFRcrea_CKDEPI_baseline"	"eGFRcrea_CKDEPI_followup"	"age_baseline"	"age_followup"
###		
###		attach(tblPhenotypes)
###		timeDiff=age_followup-age_baseline
###		
###		check.decline.variables(eGFRcrea_CKDEPI_baseline,eGFRcrea_CKDEPI_followup,timeDiff)
###		CKDi=calc_CKDi(eGFRcrea_CKDEPI_baseline,eGFRcrea_CKDEPI_followup)
###		CKDi25=calc_CKDi25(eGFRcrea_CKDEPI_baseline,eGFRcrea_CKDEPI_followup)
###		eGFRdecline=calc_eGFRdecline(eGFRcrea_CKDEPI_baseline,eGFRcrea_CKDEPI_followup,timeDiff)
###		rapid3=calc_rapid3(eGFRcrea_CKDEPI_baseline,eGFRcrea_CKDEPI_followup,timeDiff)
###

check.decline.variables=function(a_eGFRcrea_baseline, a_eGFRcrea_followup, a_time_between_baseline_and_followup_in_years){

	# define dichotomous CKDi: 1=affected; 2=unaffected
	l_crea_baseline=length(a_eGFRcrea_baseline)
	l_crea_followup=length(a_eGFRcrea_followup)
	l_crea_time_diff=length(a_time_between_baseline_and_followup_in_years)

	# very simple check, if all vectors are of same length
	if (l_crea_baseline != l_crea_followup | l_crea_followup != l_crea_time_diff)
		stop(paste(date(),"Exit in check.decline.variables: input variables eGFRcrea_baseline, eGFRcrea_followup and time_between_baseline_and_followup_in_years must have same length!"))
}

# calculate dichtomous CKDi; 1 = cases; 2 = controls
calc_CKDi = function(a_eGFRcrea_baseline, a_eGFRcrea_followup) {

	CKDi=rep(NA,length(a_eGFRcrea_baseline))
	CKDi[which(a_eGFRcrea_baseline>=60 & a_eGFRcrea_followup <60)]=1
	CKDi[which(a_eGFRcrea_baseline>=60 & a_eGFRcrea_followup >=60)]=2
	CKDi
}
	
# calculate dichotomous CKDi25: 1=cases; 2=controls
calc_CKDi25 = function(a_eGFRcrea_baseline, a_eGFRcrea_followup) {

	CKDi25=rep(NA,length(a_eGFRcrea_baseline))
	CKDi25[which(a_eGFRcrea_baseline>=60 & a_eGFRcrea_followup <= (0.75*a_eGFRcrea_baseline))]=1
	CKDi25[which(a_eGFRcrea_baseline>=60 & a_eGFRcrea_followup >=60)]=2
	CKDi25
}

# calculate continuous eGFRdecline
# eGFRdecline < 0 value means an increasing filtration rate over time.
# eGFRdecline > 0 of decline means a loss of filtration over time and 	
calc_eGFRdecline = function(a_eGFRcrea_baseline, a_eGFRcrea_followup, a_time_between_baseline_and_followup_in_years) {

	eGFRdecline=(a_eGFRcrea_baseline-a_eGFRcrea_followup)/(a_time_between_baseline_and_followup_in_years)
	eGFRdecline
}

# calculate dichotomous rapid3: 1=cases; 2=controls
# rapid3==1 means more than 3 unit decline between baseline and follow up (decline of filtration rate)
# rapid3==2 means less or equal to a 3 unit decline between baseline and follow up (small decline or increase of filtration rate)
calc_rapid3 = function(a_eGFRcrea_baseline, a_eGFRcrea_followup, a_time_between_baseline_and_followup_in_years) {

	eGFRdecline=(a_eGFRcrea_baseline-a_eGFRcrea_followup)/(a_time_between_baseline_and_followup_in_years)
	rapid3=rep(NA,length(a_eGFRcrea_baseline))
	rapid3[which(eGFRdecline >3)]=1
	rapid3[which(eGFRdecline <=3)]=2
	rapid3
}
# eof