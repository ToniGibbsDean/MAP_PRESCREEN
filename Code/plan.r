
#Questions
    #Effect of age at menarche on PQ-B score
    #Effect of age at menopause on PQ-B score
    #Effect of total reproductive age span on PQ-B score
    #Effect of hysterectomies of several varieties on PQ-B score. These include partial, total, salpingectomy, and oophorectomy.

#rules
    #excluding anyone who had psychosis prior to menopause or hysterectomy 

#data cleaning considerations
    #missing data - only include those with data for all included variables e.g., menarche and menopause dates?
        #need to fingure out N in each category - systematic missingness? how data is distributed

#covariates 
    #medications and comorbities, demographics e.g., ethnicity, age, eduction 

#analytic plan
    #n data for each variable 
    #dtributions of variables
    #summary data frame - key summary variables:
        #PQ-B - total score = total "yes" items - should range from 0-21
                #total distress = total distress (1-5) for each question - should range from 0-105
        #hysts
    #model compariosn between a set of pre hypothesised models e.g., 
        #1. lm(PQ-B ~ ageMenarche + ageMono + repoSpan + hyst1 + hyst2 + cov1 + cov2 + ..., dat)
