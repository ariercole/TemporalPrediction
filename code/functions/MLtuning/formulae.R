# specify formulae for logistic regressions 

#### 1. APACHE as single predictor
formula.APACHE <- formula(alive ~ apache_score)

#### 2. Simple formulae - each day seperate, each factor alone
formula.day1.simple <- formula(alive ~  
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
                               + day1AdrenalineTotal + day1NoradrenalineTotal + day1VasopressinTotal)

formula.day2.simple <- formula(alive ~  
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
                               + day2AdrenalineTotal + day2NoradrenalineTotal + day2VasopressinTotal)

formula.day3.simple <- formula(alive ~  
                                 age 
                               + sex 
                               + day3minMAP + day3maxMAP 
                               + day3minHR + day3maxHR 
                               + day3maxLactate + day3minLactate
                               + day3minPotassium + day3maxPotassium
                               + day3minSodium + day3maxSodium
                               + day3minPH + day3maxPH
                               + day3Ventilated
                               + day3minCreatinine + day3maxCreatinine
                               + day3minPaO2FiO2 + day3maxPaO2FiO2
                               + day3minCRP + day3maxCRP
                               + day3AdrenalineTotal + day3NoradrenalineTotal + day3VasopressinTotal)

formula.day4.simple <- formula(alive ~  
                                 age 
                               + sex 
                               + day4minMAP + day4maxMAP 
                               + day4minHR + day4maxHR 
                               + day4maxLactate + day4minLactate
                               + day4minPotassium + day4maxPotassium
                               + day4minSodium + day4maxSodium
                               + day4minPH + day4maxPH
                               + day4Ventilated
                               + day4minCreatinine + day4maxCreatinine
                               + day4minPaO2FiO2 + day4maxPaO2FiO2
                               + day4minCRP + day4maxCRP
                               + day4AdrenalineTotal + day4NoradrenalineTotal + day4VasopressinTotal)

formula.day5.simple <- formula(alive ~  
                                 age 
                               + sex 
                               + day5minMAP + day5maxMAP 
                               + day5minHR + day5maxHR 
                               + day5maxLactate + day5minLactate
                               + day5minPotassium + day5maxPotassium
                               + day5minSodium + day5maxSodium
                               + day5minPH + day5maxPH
                               + day5Ventilated
                               + day5minCreatinine + day5maxCreatinine
                               + day5minPaO2FiO2 + day5maxPaO2FiO2
                               + day5minCRP + day5maxCRP
                               + day5AdrenalineTotal + day5NoradrenalineTotal + day5VasopressinTotal)


#### 3. Custom formulae to add interactions
# add together to a complete formula, days 1, 2 & 3
formula.day1.custom <- formula(alive ~  
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
                               + day1minLactate:day1minMAP
                               + age:day1maxMAP)

formula.day2.custom <- formula(alive ~  
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
                               + day2minLactate:day2minMAP
                               + age:day2maxMAP)

formula.day3.custom <- formula(alive ~  
                                 age 
                               + sex 
                               + day3minMAP + day3maxMAP 
                               + day3minHR + day3maxHR 
                               + day3maxLactate + day3minLactate
                               + day3minPotassium + day3maxPotassium
                               + day3minSodium + day3maxSodium
                               + day3minPH + day3maxPH
                               + day3Ventilated
                               + day3minCreatinine + day3maxCreatinine
                               + day3minPaO2FiO2 + day3maxPaO2FiO2
                               + day3minCRP + day3maxCRP
                               + day3NoradrenalineTotal + day3VasopressinTotal + day3AdrenalineTotal
                               + day3minLactate:day3NoradrenalineTotal
                               + day3minLactate:day3minMAP
                               + age:day3maxMAP)

formula.day4.custom <- formula(alive ~  
                                 age 
                               + sex 
                               + day4minMAP + day4maxMAP 
                               + day4minHR + day4maxHR 
                               + day4maxLactate + day4minLactate
                               + day4minPotassium + day4maxPotassium
                               + day4minSodium + day4maxSodium
                               + day4minPH + day4maxPH
                               + day4Ventilated
                               + day4minCreatinine + day4maxCreatinine
                               + day4minPaO2FiO2 + day4maxPaO2FiO2
                               + day4minCRP + day4maxCRP
                               + day4NoradrenalineTotal + day4VasopressinTotal + day4AdrenalineTotal
                               + day4minLactate:day4NoradrenalineTotal
                               + day4minLactate:day4minMAP
                               + age:day4maxMAP)

formula.day5.custom <- formula(alive ~  
                                 age 
                               + sex 
                               + day5minMAP + day5maxMAP 
                               + day5minHR + day5maxHR 
                               + day5maxLactate + day5minLactate
                               + day5minPotassium + day5maxPotassium
                               + day5minSodium + day5maxSodium
                               + day5minPH + day5maxPH
                               + day5Ventilated
                               + day5minCreatinine + day5maxCreatinine
                               + day5minPaO2FiO2 + day5maxPaO2FiO2
                               + day5minCRP + day5maxCRP
                               + day5NoradrenalineTotal + day5VasopressinTotal + day5AdrenalineTotal
                               + day5minLactate:day5NoradrenalineTotal
                               + day5minLactate:day5minMAP
                               + age:day5maxMAP)

#### 4. Cumulative formulae

formula.day12.simple<- formula(alive ~  
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
                               + day1AdrenalineTotal + day1NoradrenalineTotal + day1VasopressinTotal
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
                               + day2AdrenalineTotal + day2NoradrenalineTotal + day2VasopressinTotal)


formula.day123.simple<- formula(alive ~  
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
                                + day1AdrenalineTotal + day1NoradrenalineTotal + day1VasopressinTotal
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
                                + day2AdrenalineTotal + day2NoradrenalineTotal + day2VasopressinTotal
                                + day3minMAP + day3maxMAP 
                                + day3minHR + day3maxHR 
                                + day3maxLactate + day3minLactate
                                + day3minPotassium + day3maxPotassium
                                + day3minSodium + day3maxSodium
                                + day3minPH + day3maxPH
                                + day3Ventilated
                                + day3minCreatinine + day3maxCreatinine
                                + day3minPaO2FiO2 + day3maxPaO2FiO2
                                + day3minCRP + day3maxCRP
                                + day3AdrenalineTotal + day3NoradrenalineTotal + day3VasopressinTotal)

formula.day1234.simple<- formula(alive ~  
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
                                + day1AdrenalineTotal + day1NoradrenalineTotal + day1VasopressinTotal
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
                                + day2AdrenalineTotal + day2NoradrenalineTotal + day2VasopressinTotal
                                + day3minMAP + day3maxMAP 
                                + day3minHR + day3maxHR 
                                + day3maxLactate + day3minLactate
                                + day3minPotassium + day3maxPotassium
                                + day3minSodium + day3maxSodium
                                + day3minPH + day3maxPH
                                + day3Ventilated
                                + day3minCreatinine + day3maxCreatinine
                                + day3minPaO2FiO2 + day3maxPaO2FiO2
                                + day3minCRP + day3maxCRP
                                + day3AdrenalineTotal + day3NoradrenalineTotal + day3VasopressinTotal
                                + day4minMAP + day4maxMAP 
                                + day4minHR + day4maxHR 
                                + day4maxLactate + day4minLactate
                                + day4minPotassium + day4maxPotassium
                                + day4minSodium + day4maxSodium
                                + day4minPH + day4maxPH
                                + day4Ventilated
                                + day4minCreatinine + day4maxCreatinine
                                + day4minPaO2FiO2 + day4maxPaO2FiO2
                                + day4minCRP + day4maxCRP
                                + day4NoradrenalineTotal + day4VasopressinTotal + day4AdrenalineTotal)

formula.day12345.simple<- formula(alive ~  
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
                                 + day1AdrenalineTotal + day1NoradrenalineTotal + day1VasopressinTotal
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
                                 + day2AdrenalineTotal + day2NoradrenalineTotal + day2VasopressinTotal
                                 + day3minMAP + day3maxMAP 
                                 + day3minHR + day3maxHR 
                                 + day3maxLactate + day3minLactate
                                 + day3minPotassium + day3maxPotassium
                                 + day3minSodium + day3maxSodium
                                 + day3minPH + day3maxPH
                                 + day3Ventilated
                                 + day3minCreatinine + day3maxCreatinine
                                 + day3minPaO2FiO2 + day3maxPaO2FiO2
                                 + day3minCRP + day3maxCRP
                                 + day3AdrenalineTotal + day3NoradrenalineTotal + day3VasopressinTotal
                                 + day4minMAP + day4maxMAP 
                                 + day4minHR + day4maxHR 
                                 + day4maxLactate + day4minLactate
                                 + day4minPotassium + day4maxPotassium
                                 + day4minSodium + day4maxSodium
                                 + day4minPH + day4maxPH
                                 + day4Ventilated
                                 + day4minCreatinine + day4maxCreatinine
                                 + day4minPaO2FiO2 + day4maxPaO2FiO2
                                 + day4minCRP + day4maxCRP
                                 + day4NoradrenalineTotal + day4VasopressinTotal + day4AdrenalineTotal
                                 + day5minMAP + day5maxMAP 
                                 + day5minHR + day5maxHR 
                                 + day5maxLactate + day5minLactate
                                 + day5minPotassium + day5maxPotassium
                                 + day5minSodium + day5maxSodium
                                 + day5minPH + day5maxPH
                                 + day5Ventilated
                                 + day5minCreatinine + day5maxCreatinine
                                 + day5minPaO2FiO2 + day5maxPaO2FiO2
                                 + day5minCRP + day5maxCRP
                                 + day5NoradrenalineTotal + day5VasopressinTotal + day5AdrenalineTotal)

formula.day12.custom<- formula(alive ~  
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
                               + day1AdrenalineTotal + day1NoradrenalineTotal + day1VasopressinTotal
                               + day1minLactate:day1NoradrenalineTotal
                               + day1minLactate:day1minMAP
                               + age:day1maxMAP
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
                               + day2AdrenalineTotal + day2NoradrenalineTotal + day2VasopressinTotal
                               + day2minLactate:day2NoradrenalineTotal
                               + day2minLactate:day2minMAP
                               + age:day2maxMAP)

formula.day123.custom<- formula(alive ~  
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
                                + day1AdrenalineTotal + day1NoradrenalineTotal + day1VasopressinTotal
                                + day1minLactate:day1NoradrenalineTotal
                                + day1minLactate:day1minMAP
                                + age:day1maxMAP
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
                                + day2AdrenalineTotal + day2NoradrenalineTotal + day2VasopressinTotal
                                + day2minLactate:day2NoradrenalineTotal
                                + day2minLactate:day2minMAP
                                + age:day2maxMAP
                                + day3minMAP + day3maxMAP 
                                + day3minHR + day3maxHR 
                                + day3maxLactate + day3minLactate
                                + day3minPotassium + day3maxPotassium
                                + day3minSodium + day3maxSodium
                                + day3minPH + day3maxPH
                                + day3Ventilated
                                + day3minCreatinine + day3maxCreatinine
                                + day3minPaO2FiO2 + day3maxPaO2FiO2
                                + day3minCRP + day3maxCRP
                                + day3AdrenalineTotal + day3NoradrenalineTotal + day3VasopressinTotal
                                + day3minLactate:day3NoradrenalineTotal
                                + day3minLactate:day3minMAP
                                + age:day3maxMAP)


formula.day1234.custom<- formula(alive ~  
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
                                 + day1AdrenalineTotal + day1NoradrenalineTotal + day1VasopressinTotal
                                 + day1minLactate:day1NoradrenalineTotal
                                 + day1minLactate:day1minMAP
                                 + age:day1maxMAP
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
                                 + day2AdrenalineTotal + day2NoradrenalineTotal + day2VasopressinTotal
                                 + day2minLactate:day2NoradrenalineTotal
                                 + day2minLactate:day2minMAP
                                 + age:day2maxMAP
                                 + day3minMAP + day3maxMAP 
                                 + day3minHR + day3maxHR 
                                 + day3maxLactate + day3minLactate
                                 + day3minPotassium + day3maxPotassium
                                 + day3minSodium + day3maxSodium
                                 + day3minPH + day3maxPH
                                 + day3Ventilated
                                 + day3minCreatinine + day3maxCreatinine
                                 + day3minPaO2FiO2 + day3maxPaO2FiO2
                                 + day3minCRP + day3maxCRP
                                 + day3AdrenalineTotal + day3NoradrenalineTotal + day3VasopressinTotal
                                 + day3minLactate:day3NoradrenalineTotal
                                 + day3minLactate:day3minMAP
                                 + age:day3maxMAP
                                 + day4minMAP + day4maxMAP 
                                 + day4minHR + day4maxHR 
                                 + day4maxLactate + day4minLactate
                                 + day4minPotassium + day4maxPotassium
                                 + day4minSodium + day4maxSodium
                                 + day4minPH + day4maxPH
                                 + day4Ventilated
                                 + day4minCreatinine + day4maxCreatinine
                                 + day4minPaO2FiO2 + day4maxPaO2FiO2
                                 + day4minCRP + day4maxCRP
                                 + day4NoradrenalineTotal + day4VasopressinTotal + day4AdrenalineTotal
                                 + day4minLactate:day4NoradrenalineTotal
                                 + day4minLactate:day4minMAP
                                 + age:day4maxMAP)


formula.day12345.custom<- formula(alive ~  
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
                                  + day1AdrenalineTotal + day1NoradrenalineTotal + day1VasopressinTotal
                                  + day1minLactate:day1NoradrenalineTotal
                                  + day1minLactate:day1minMAP
                                  + age:day1maxMAP
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
                                  + day2AdrenalineTotal + day2NoradrenalineTotal + day2VasopressinTotal
                                  + day2minLactate:day2NoradrenalineTotal
                                  + day2minLactate:day2minMAP
                                  + age:day2maxMAP
                                  + day3minMAP + day3maxMAP 
                                  + day3minHR + day3maxHR 
                                  + day3maxLactate + day3minLactate
                                  + day3minPotassium + day3maxPotassium
                                  + day3minSodium + day3maxSodium
                                  + day3minPH + day3maxPH
                                  + day3Ventilated
                                  + day3minCreatinine + day3maxCreatinine
                                  + day3minPaO2FiO2 + day3maxPaO2FiO2
                                  + day3minCRP + day3maxCRP
                                  + day3AdrenalineTotal + day3NoradrenalineTotal + day3VasopressinTotal
                                  + day3minLactate:day3NoradrenalineTotal
                                  + day3minLactate:day3minMAP
                                  + age:day3maxMAP
                                  + day4minMAP + day4maxMAP 
                                  + day4minHR + day4maxHR 
                                  + day4maxLactate + day4minLactate
                                  + day4minPotassium + day4maxPotassium
                                  + day4minSodium + day4maxSodium
                                  + day4minPH + day4maxPH
                                  + day4Ventilated
                                  + day4minCreatinine + day4maxCreatinine
                                  + day4minPaO2FiO2 + day4maxPaO2FiO2
                                  + day4minCRP + day4maxCRP
                                  + day4NoradrenalineTotal + day4VasopressinTotal + day4AdrenalineTotal
                                  + day4minLactate:day4NoradrenalineTotal
                                  + day4minLactate:day4minMAP
                                  + age:day4maxMAP
                                  + day5minMAP + day5maxMAP 
                                  + day5minHR + day5maxHR 
                                  + day5maxLactate + day5minLactate
                                  + day5minPotassium + day5maxPotassium
                                  + day5minSodium + day5maxSodium
                                  + day5minPH + day5maxPH
                                  + day5Ventilated
                                  + day5minCreatinine + day5maxCreatinine
                                  + day5minPaO2FiO2 + day5maxPaO2FiO2
                                  + day5minCRP + day5maxCRP
                                  + day5NoradrenalineTotal + day5VasopressinTotal + day5AdrenalineTotal
                                  + day5minLactate:day5NoradrenalineTotal
                                  + day5minLactate:day5minMAP
                                  + age:day5maxMAP)