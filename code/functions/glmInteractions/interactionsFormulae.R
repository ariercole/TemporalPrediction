# formulae used in testing of interactions for glm

formula.lacAge.day1 <- formula(alive ~  
                                 age 
                               + sex 
                               + day1minMAP + day1maxMAP 
                               + day1minHR + day1maxHR 
                               + day1maxLactate + day1minLactate
                               + day1minPotassium + day1maxPotassium
                               + day1minSodium + day1maxSodium
                               + day1minPH + day1maxPH
                               + day1Ventilated
                               + day1minCreatinine + day1maxCreatinine
                               + day1minPaO2FiO2 + day1maxPaO2FiO2
                               + day1minCRP + day1maxCRP
                               + day1NoradrenalineTotal + day1VasopressinTotal + day1AdrenalineTotal
                               + age:day1maxMAP
                               + age:day1minMAP)

formula.lacNorad.day1 <- formula(alive ~  
                                   age 
                                 + sex 
                                 + day1minMAP + day1maxMAP 
                                 + day1minHR + day1maxHR 
                                 + day1maxLactate + day1minLactate
                                 + day1minPotassium + day1maxPotassium
                                 + day1minSodium + day1maxSodium
                                 + day1minPH + day1maxPH
                                 + day1Ventilated
                                 + day1minCreatinine + day1maxCreatinine
                                 + day1minPaO2FiO2 + day1maxPaO2FiO2
                                 + day1minCRP + day1maxCRP
                                 + day1NoradrenalineTotal + day1VasopressinTotal + day1AdrenalineTotal
                                 + day1minLactate:day1NoradrenalineTotal
                                 + day1maxLactate:day1NoradrenalineTotal)

formula.lacMAP.day1 <- formula(alive ~  
                                 age 
                               + sex 
                               + day1minMAP + day1maxMAP 
                               + day1minHR + day1maxHR 
                               + day1maxLactate + day1minLactate
                               + day1minPotassium + day1maxPotassium
                               + day1minSodium + day1maxSodium
                               + day1minPH + day1maxPH
                               + day1Ventilated
                               + day1minCreatinine + day1maxCreatinine
                               + day1minPaO2FiO2 + day1maxPaO2FiO2
                               + day1minCRP + day1maxCRP
                               + day1NoradrenalineTotal + day1VasopressinTotal + day1AdrenalineTotal
                               + day1minLactate:day1maxMAP
                               + day1minLactate:day1minMAP
                               + day1maxLactate:day1maxMAP
                               + day1maxLactate:day1minMAP)


### Day 2
formula.lacAge.day2 <- formula(alive ~  
                                 age 
                               + sex 
                               + day2minMAP + day2maxMAP 
                               + day2minHR + day2maxHR 
                               + day2maxLactate + day2minLactate
                               + day2minPotassium + day2maxPotassium
                               + day2minSodium + day2maxSodium
                               + day2minPH + day2maxPH
                               + day2Ventilated
                               + day2minCreatinine + day2maxCreatinine
                               + day2minPaO2FiO2 + day2maxPaO2FiO2
                               + day2minCRP + day2maxCRP
                               + day2NoradrenalineTotal + day2VasopressinTotal + day2AdrenalineTotal
                               + age:day2maxMAP
                               + age:day2minMAP)

formula.lacNorad.day2 <- formula(alive ~  
                                   age 
                                 + sex 
                                 + day2minMAP + day2maxMAP 
                                 + day2minHR + day2maxHR 
                                 + day2maxLactate + day2minLactate
                                 + day2minPotassium + day2maxPotassium
                                 + day2minSodium + day2maxSodium
                                 + day2minPH + day2maxPH
                                 + day2Ventilated
                                 + day2minCreatinine + day2maxCreatinine
                                 + day2minPaO2FiO2 + day2maxPaO2FiO2
                                 + day2minCRP + day2maxCRP
                                 + day2NoradrenalineTotal + day2VasopressinTotal + day2AdrenalineTotal
                                 + day2minLactate:day2NoradrenalineTotal
                                 + day2maxLactate:day2NoradrenalineTotal)

formula.lacMAP.day2 <- formula(alive ~  
                                 age 
                               + sex 
                               + day2minMAP + day2maxMAP 
                               + day2minHR + day2maxHR 
                               + day2maxLactate + day2minLactate
                               + day2minPotassium + day2maxPotassium
                               + day2minSodium + day2maxSodium
                               + day2minPH + day2maxPH
                               + day2Ventilated
                               + day2minCreatinine + day2maxCreatinine
                               + day2minPaO2FiO2 + day2maxPaO2FiO2
                               + day2minCRP + day2maxCRP
                               + day2NoradrenalineTotal + day2VasopressinTotal + day2AdrenalineTotal
                               + day2minLactate:day2maxMAP
                               + day2minLactate:day2minMAP
                               + day2maxLactate:day2maxMAP
                               + day2maxLactate:day2minMAP)
