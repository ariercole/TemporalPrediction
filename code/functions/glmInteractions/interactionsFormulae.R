### Day 1
formula.allInt.day1 <- formula(alive ~  
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
                               + age:day1minMAP
                               + day1minLactate:day1NoradrenalineTotal
                               + day1maxLactate:day1NoradrenalineTotal
                               + day1minLactate:day1maxMAP
                               + day1minLactate:day1minMAP
                               + day1maxLactate:day1maxMAP
                               + day1maxLactate:day1minMAP)

formula.selectInt.day1 <- formula(alive ~  
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
                               + day1minLactate:day1NoradrenalineTotal
                               + day1minLactate:day1minMAP)

