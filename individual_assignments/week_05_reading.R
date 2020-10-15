#Reading Questions Week 5

#clear workspace
rm(list = ls())

#from Bolker 1.4
pol_n_predation = 26
pol_n_no_predation = 184
pol_n_total = sum(pol_n_predation,pol_n_no_predation)
pol_predation_rate = pol_n_predation/pol_n_total
  
psd_n_predation = 25
psd_n_no_predation = 706
psd_n_total = sum(psd_n_predation,psd_n_no_predation)
psd_predation_rate = psd_n_predation/psd_n_total

#self-check
print(
  paste0(
    "The seed predation rate for Polyscias fulva is: ",
    round(pol_predation_rate, digits = 3)))

print(
  paste0(
    "The seed predation rate for Pseudospondias microcarpa is: ",
    round(psd_predation_rate, digits = 3)))

#determine the ratios of seed proportions
seed_prop = pol_predation_rate/psd_predation_rate

print(
  paste0(
    "The ratios of seed proportions is: ",
    round(seed_prop, digits = 3)))
