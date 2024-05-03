
#coverage
coverage <- seq(0,1,0.1)

#oviposition for resistant mosquitoes
eggs_baseline= 100
mortality_baseline = 0.18
gonotrophic_baseline = 4
oviposition_resistant = eggs_baseline/((exp(mortality_baseline *gonotrophic_baseline))-1)
print(oviposition_resistant)
oviposition_resistant_vec <- rep(oviposition_resistant, length(coverage))

#to check length
length(coverage) == length(oviposition_resistant)

#plot of resistance and coverage
plot(coverage, oviposition_resistant_vec, xlab="Coverage", ylab="Oviposition Rate",type="l")
title(main="Oviposition Rate of Resistant Mosquitoes Based on Coverage")

#dose options
dose_high.p <- 7.70
dose_control.p <- 8.03
dose_low.p<-7.97
dose_high.f <- 7.55
dose_control.f <- 7.00
dose_low.f<- 7.37
    
#parameters
repellency_effect= (1/(exp(dose_control.p+dose_high.p)) + (1/exp(dose_control.f + dose_high.f)))
#repellency_effect=(1/(exp(dose_control.p+dose_low.p)) + (1/exp(dose_control.f + dose_low.f)))
mortality_treated= exp(-0.58+2.15)
bf_baseline= .76
rate_treated= .19
rate_untreated= .5
time_transit= .3
biting_rate= 0.75

#calculating mortality rate based on coverage
mortality_sus= function(coverage,repellency_effect, time_transit, rate_treated,rate_untreated,transit_scalar, 
                        mortality_untreated, mortality_treated){
  
  return(mortality_sus)
}


Pies=function(coverage, repellency_effect, time_transit, rate_treated, rate_untreated){
  pi_transit = -(time_transit*rate_treated*rate_untreated) / (rate_treated*(coverage-repellency_effect*rate_untreated-1))
  + (coverage*(repellency_effect -1)*rate_untreated)
  pi_untreated = ((1-coverage)*pi_transit) / (time_transit * rate_untreated)
  pi_treated = ((1-repellency_effect)*coverage*pi_transit) / (time_transit * rate_treated)
  pi_df <- data.frame(pi_transit, pi_untreated, pi_treated)
  return(pi_df)
}


                 
Pi= Pies(coverage,repellency_effect,time_transit,rate_treated,rate_untreated)
  g= Pi$pi_transit*(mortality_treated)


#gonotrophic cycle for a susceptible mosquito calculation
gonotrophic_sus = function(coverage,repellency_effect, biting_rate, bf_baseline, rate_treated, rate_untreated){
  
  prob_fail_treated= (rate_treated)/((biting_rate * bf_baseline) + rate_treated)
  prob_fail_untreated= (rate_untreated)/(biting_rate + rate_untreated)
  D = coverage * repellency_effect + coverage * (1-repellency_effect) * prob_fail_untreated + (1-coverage) * prob_fail_untreated
  geom_mean = D / (1-D)
  delta = (time_transit * coverage * repellency_effect + (1/rate_treated + time_transit) * (coverage * (1-repellency_effect) * prob_fail_treated) + 
             (1/prob_fail_untreated + time_transit) * ((1-coverage) * prob_fail_untreated)) / D
  gonotrophic_sus = 1/ (geom_mean*delta + 1/biting_rate)
  return(gonotrophic_sus)
}


#oviposition for susceptible mosquitoes
eggs_sus= 100
mortality_sus= function(coverage,repellency_effect, time_transit, rate_treated,rate_untreated,transit_scalar, 
                        mortality_untreated, mortality_treated){
  
  return(mortality_sus)
}

oviposition_sus = eggs_sus/((exp(mortality_sus * gonotrophic_sus))-1)
print(oviposition_sus)
oviposition_sus_vec <- rep(oviposition_sus, length(coverage))

#plot of susceptible and coverage
plot(coverage, oviposition_sus_vec, xlab="Coverage", ylab="Oviposition Rate",type="l")
title(main="Oviposition Rate of Susceptible Mosquitoes Based on Coverage")


