# functions to enable age group and sex to be selected
inla.function.climate.3var <- function(age.sel,sex.sel,year.start,year.end,type,cluster,cause='Allcause') {
    
    dat.inla <- dat.merged
    
    # filter all data by sex age and month
    sex <- sex.sel
    age <- age.sel
    fit.years <- year.start:year.end
    dat.inla <- dat.inla[dat.inla$sex==sex & dat.inla$age==age & dat.inla$year %in% fit.years,]
    
    # load drawseq lookup
    drawseq.lookup <-readRDS('~/git/mortality/USA/state/output/adj_matrix_create/drawseq.lookup.rds')
    
    #dat.inla <- merge(dat.inla,drawseq.lookup, by='fips')
    
    # extract unique table of year and months to generate year.month
    dat.year.month <- unique(dat.inla[,c('year', 'month')])
    dat.year.month$month <- as.integer(dat.year.month$month)
    dat.year.month$year.month <- seq(nrow(dat.year.month))
    
    # merge year.month table with population table to create year.month id
    dat.inla <- merge(dat.inla,dat.year.month, by=c('year','month'))
    
    # make sure that the order of the main data file matches that of the shapefile,
    # otherwise the model will not be valid
    dat.inla <- dat.inla[order(dat.inla$DRAWSEQ,dat.inla$sex,dat.inla$age,dat.inla$year.month),]
    
    # add ID column for INLA
    dat.inla$ID <- dat.inla$DRAWSEQ
    
    # fix rownames
    rownames(dat.inla) <- 1:nrow(dat.inla)
    
    # variables for INLA model
    
    dat.inla$year.month4 <- dat.inla$year.month3 <- dat.inla$year.month2 <- dat.inla$year.month
    dat.inla$month7 <- dat.inla$month6 <- dat.inla$month5 <- dat.inla$month4 <- dat.inla$month3 <- dat.inla$month2 <- dat.inla$month
    dat.inla$ID5 <- dat.inla$ID4 <- dat.inla$ID3 <- dat.inla$ID2 <- dat.inla$ID
    dat.inla$e <- 1:nrow(dat.inla)
    
    # INLA

    if(type==17){

        # 1. Type Id space-time interaction with besag state interaction terms and state-month specific variable slope (iid)
        # NO MONTH TERMS AND NO RW1
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        #f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        #f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        #f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        #f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable1, model="iid") +                                              # month specific climate slope
        f(month6, variable2, model="iid") +                                              # month specific climate slope
        f(month7, variable3, model="iid") +                                              # month specific climate slope
        # random walk across time
        #f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }

    if(type==16){

        # 1. Type Id space-time interaction with besag state interaction terms and state-month specific variable slope (iid)
        # NO MONTH TERMS
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        #f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        #f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        #f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        #f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable1, model="iid") +                                              # month specific climate slope
        f(month6, variable2, model="iid") +                                              # month specific climate slope
        f(month7, variable3, model="iid") +                                              # month specific climate slope
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }

    if(type==1){

        # 1. Type I space-time interaction
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                            		# global slope
        # month specific terms
        f(month, model="rw1",cyclic = TRUE) +                                   		# month specific intercept
        f(month2, year.month2, model="rw1",cyclic = TRUE) +                     		# month specific slope
        # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable1, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        f(month6, variable2, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        f(month7, variable3, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        #f(month5, variable, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific climate slope (spatially-correlated)
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                     		# overdispersion term
    }

    if(type==2){

        # 1. Type Ia space-time interaction
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable1, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        f(month6, variable2, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        f(month7, variable3, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        #f(month5, variable, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific climate slope (spatially-correlated)
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }

    if(type==3) {

        # 2. Type II space-time interaction
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                            		# global slope
        # month specific terms
        f(month, model="rw1",cyclic = TRUE) +                                   		# month specific intercept
        f(month2, year.month2, model="rw1",cyclic = TRUE) +                     		# month specific slope
        # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable1, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        f(month6, variable2, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        f(month7, variable3, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        #f(month5, variable, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific climate slope (spatially-correlated)
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # space-time interaction
        f(ID3,model="iid", group=year.month4,                                   		# type II space-time interaction
        control.group=list(model="rw1")) +
        #control.group=list(model="rw2")) +
        #control.group=list(model="exchangeable")) +
        # overdispersion term
        f(e, model = "iid")                                                     		# overdispersion term
    }

    if(type==4) {

        # 2. Type IIa space-time interaction
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                             # global intercept
        year.month +                                                                    # global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                              # state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                                # state specific slope (BYM)
        # climate specific terms
        f(month5, variable1, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        f(month6, variable2, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        f(month7, variable3, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        #f(month5, variable, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific climate slope (spatially-correlated)
        # random walk across time
        f(year.month3, model="rw1") +                                                   # rw1
        # space-time interaction
        f(ID3,model="iid", group=year.month4,                                           # type II space-time interaction
        control.group=list(model="rw1")) +
        #control.group=list(model="rw2")) +
        #control.group=list(model="exchangeable")) +
        # overdispersion term
        f(e, model = "iid")                                                             # overdispersion term
    }

    if(type==5) {

        # 3. Type III space-time interaction
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           		 	# global slope
        # month specific terms
        f(month, model="rw1",cyclic = TRUE) +                                   		# month specific intercept
        f(month2, year.month2, model="rw1",cyclic = TRUE) +                     		# month specific slope
        # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable1, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        f(month6, variable2, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        f(month7, variable3, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        #f(month5, variable, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific climate slope (spatially-correlated)
        # random walk terms
        f(year.month3, model="rw1") +                                           		# rw1
        #f(year.month4,model="iid", group=ID3,                                          # type III space-time interaction
        f(year.month4,model="rw1", group=ID3,                                           # variation on model
        control.group=list(model="I")) +
        #control.group=list(model="besag",                                              # variation on model
        #graph=USA.adj)) +                                                              # variation on model
        # overdispersion term
        f(e, model = "iid")                                                     		# overdispersion term
    }

    if(type==6) {

        # 3. Type IIIa space-time interaction
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           		 	# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable1, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        f(month6, variable2, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        f(month7, variable3, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        #f(month5, variable, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific climate slope (spatially-correlated)
        #f(year.month3, model="rw1") +                                           		# rw1 across time (put back?)
        #f(year.month4,model="iid", group=ID3,                                          # type III space-time interaction
        f(year.month4,model="rw1", group=ID3,                                           # variation on model
        control.group=list(model="I")) +
        #control.group=list(model="besag",                                              # variation on model
        #graph=USA.adj)) +                                                              # variation on model
        # overdispersion term
        f(e, model = "iid")                                                     		# overdispersion term
    }

    if(type==7) {

        # 4. Type IV space-time interaction

        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                            		# global slope
        # month specific terms
        f(month, model="rw1",cyclic = TRUE) +                                   		# month specific intercept
        f(month2, year.month2, model="rw1",cyclic = TRUE) +                     		# month specific slope
        # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                     		 	# state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable1, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        f(month6, variable2, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        f(month7, variable3, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        #f(month5, variable, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific climate slope (spatially-correlated)
        # random walk terms
        f(year.month3, model="rw1") +                                           		# rw1
        f(ID3,model="besag", graph=USA.adj,                                     		# type IV space-time interaction
        group=year.month4,
        control.group=list(model="rw1")) +
        # overdispersion term
        f(e, model = "iid")                                                     		# overdispersion term
    }

    if(type==8){

        # 1. Type Ib space-time interaction with besag state interaction terms and national variable slope
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        variable1 +                                                                      # national variable slope
        variable2 +                                                                      # national variable slope
        variable3 +                                                                      # national variable slope
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }

    if(type==9){

        # 1. Type Ic space-time interaction with besag state interaction terms and state specific variable slope
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(ID3, variable1, model='iid') +                                                 # state specific climate slope
        f(ID4, variable2, model='iid') +                                                 # state specific climate slope
        f(ID5, variable3, model='iid') +                                                 # state specific climate slope
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }

    if(type==10){

        # 1. Type Id space-time interaction with besag state interaction terms and state-month specific variable slope (rw1)
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable1, model="rw1", cyclic=TRUE) +                                              # month specific climate slope
        f(month6, variable2, model="rw1", cyclic=TRUE) +                                              # month specific climate slope
        f(month7, variable3, model="rw1", cyclic=TRUE) +                                              # month specific climate slope
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }

    if(type==11){

        # 1. Type Ie space-time interaction with besag state interaction terms and state-month specific variable slope (rw1)
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable1, model="rw1",cyclic = TRUE, group=ID) +                      # state-month specific climate slope
        f(month6, variable2, model="rw1",cyclic = TRUE, group=ID) +                      # state-month specific climate slope
        f(month7, variable3, model="rw1",cyclic = TRUE, group=ID) +                      # state-month specific climate slope
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }



    if(type==12){

        # 1. Type If space-time interaction with besag state interaction terms and spatially-correlated state-month specific variable slope (rw1)
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable1, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific climate slope (spatially-correlated)
        f(month6, variable2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific climate slope (spatially-correlated)
        f(month7, variable3, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific climate slope (spatially-correlated)
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }

    if(type==13){

        # 1. Type Ief space-time interaction with besag state interaction terms and spatially-correlated state-month specific variable slope (rw1)
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(ID3, variable, model='iid') +                                                 # state specific climate slope
        f(month5, variable1, model="iid") +                                              # month specific climate slope
        f(month6, variable2, model="iid") +                                              # month specific climate slope
        f(month7, variable3, model="iid") +                                              # month specific climate slope
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }

    if(type==14){

        # 1. Type Ide space-time interaction with besag state interaction terms and separate month- and state-specific variable slope (iid)
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(ID3, variable1, model='iid') +                                                 # state specific climate slope
        f(ID4, variable2, model='iid') +                                                 # state specific climate slope
        f(month5, variable1, model="iid") +                                              # month specific climate slope
        f(month6, variable2, model="iid") +                                              # month specific climate slope
        f(month7, variable3, model="iid") +                                              # month specific climate slope
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }

    if(type==15){

        # 1. Type Ig space-time interaction with besag state interaction terms and region-month specific variable slope (rw1)
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable1, model="rw1",cyclic = TRUE, group=ID.clim) +                 # state-month specific climate slope
        f(month6, variable2, model="rw1",cyclic = TRUE, group=ID.clim) +                 # state-month specific climate slope
        f(month7, variable3, model="rw1",cyclic = TRUE, group=ID.clim) +                 # state-month specific climate slope
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }
    
    # INLA model
    system.time(mod <-
    inla(formula = fml,
    family = "poisson",
    data = dat.inla,
    E = pop.adj,
    control.compute = list(dic=TRUE),
    control.predictor = list(link = 1),
    #verbose=TRUE
    ))
    
    # create directory for output
    file.loc <- paste0('~/data/mortality/US/state/climate_effects/',dname.arg,'/3var/',metric.arg,'/non_pw/type_',type.selected,'/age_groups/',age.sel)
    ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)
    
    if(cod.arg!='AllCause'){
        # save all parameters of INLA model
        parameters.name <- paste0('USA_rate_pred_type',type.selected,'_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end,'_',dname.arg,'_',metric.arg,'_',cause,'_parameters')
        #mod$misc <- NULL
        #mod$.args$.parent.frame <- NULL
        if(cluster==0){saveRDS(mod,paste0(file.loc,'/',parameters.name))}
        if(cluster==1){saveRDS(mod,paste0('../output/pred/',parameters.name))}

        # save summary of INLA model
        summary.name <- paste0('USA_rate_pred_type',type.selected,'_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end,'_',dname.arg,'_',metric.arg,'_',cause,'_summary.txt')
        inla.summary.mod <- summary(mod)
        if(cluster==0){capture.output(inla.summary.mod,file=paste0(file.loc,'/',summary.name))}
        if(cluster==1){capture.output(inla.summary.mod,file=paste0('../output/summary/',summary.name))}

        # capture output for emailing purposes
        email.content <- capture.output(inla.summary.mod)

        # save RDS of INLA results
        plot.dat <- as.data.frame(cbind(dat.inla,rate.pred=mod$summary.fitted.values$mean,sd=mod$summary.fitted.values$sd))

        # name of RDS output file then save
        RDS.name <- paste0('USA_rate_pred_type',type.selected,'_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end,'_',dname.arg,'_',metric.arg,'_',cause)
        if(cluster==0){saveRDS(plot.dat,paste0(file.loc,'/',RDS.name))}
        if(cluster==1){saveRDS(plot.dat,paste0('../output/pred/',RDS.name))}
    }

    if(cod.arg =='AllCause'){
        # save all parameters of INLA model
        parameters.name <- paste0('USA_rate_pred_type',type.selected,'_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end,'_',dname.arg,'_',metric.arg,'_parameters')
        #mod$misc <- NULL
        #mod$.args$.parent.frame <- NULL
        if(cluster==0){saveRDS(mod,paste0(file.loc,'/',parameters.name))}
        if(cluster==1){saveRDS(mod,paste0('../output/pred/',parameters.name))}

        # save summary of INLA model
        summary.name <- paste0('USA_rate_pred_type',type.selected,'_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end,'_',dname.arg,'_',metric.arg,'_summary.txt')
        inla.summary.mod <- summary(mod)
        if(cluster==0){capture.output(inla.summary.mod,file=paste0(file.loc,'/',summary.name))}
        if(cluster==1){capture.output(inla.summary.mod,file=paste0('../output/summary/',summary.name))}

        # capture output for emailing purposes
        email.content <- capture.output(inla.summary.mod)

        # save RDS of INLA results
        plot.dat <- as.data.frame(cbind(dat.inla,rate.pred=mod$summary.fitted.values$mean,sd=mod$summary.fitted.values$sd))

        # name of RDS output file then save
        RDS.name <- paste0('USA_rate_pred_type',type.selected,'_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end,'_',dname.arg,'_',metric.arg)
        if(cluster==0){saveRDS(plot.dat,paste0(file.loc,'/',RDS.name))}
        if(cluster==1){saveRDS(plot.dat,paste0('../output/pred/',RDS.name))}
    }
    
    sender <- "emailr349@gmail.com"
    recipients <- c("r.parks15@imperial.ac.uk")
    send.mail(from = sender,
    to = recipients,
    subject = paste0(sex.lookup[sex.sel],' ',age.sel,' model ',type.selected,' ',dname.arg,' ',metric.arg,'_',cause,' non-pw done'),
    body = "Well done",
    smtp = list(host.name = "smtp.gmail.com", port = 465,
    user.name = "emailr349@gmail.com",
    passwd = "inlaisthebest", ssl = TRUE),
    authenticate = TRUE,
    send = TRUE)

    
    # this bracket ends the function at the top of the script
}

# functions to enable age group and sex to be selected with rough run to improve speed
inla.function.climate.3var.fast <- function(age.sel,sex.sel,year.start,year.end,type,cluster,cause='Allcause') {
    
    dat.inla <- dat.merged
    
    # filter all data by sex age and month
    sex <- sex.sel
    age <- age.sel
    fit.years <- year.start:year.end
    dat.inla <- dat.inla[dat.inla$sex==sex & dat.inla$age==age & dat.inla$year %in% fit.years,]
    
    # load drawseq lookup
    drawseq.lookup <-readRDS('~/git/mortality/USA/state/output/adj_matrix_create/drawseq.lookup.rds')
    
    #dat.inla <- merge(dat.inla,drawseq.lookup, by='fips')
    
    # extract unique table of year and months to generate year.month
    dat.year.month <- unique(dat.inla[,c('year', 'month')])
    dat.year.month$month <- as.integer(dat.year.month$month)
    dat.year.month$year.month <- seq(nrow(dat.year.month))
    
    # merge year.month table with population table to create year.month id
    dat.inla <- merge(dat.inla,dat.year.month, by=c('year','month'))
    
    # make sure that the order of the main data file matches that of the shapefile,
    # otherwise the model will not be valid
    dat.inla <- dat.inla[order(dat.inla$DRAWSEQ,dat.inla$sex,dat.inla$age,dat.inla$year.month),]
    
    # add ID column for INLA
    dat.inla$ID <- dat.inla$DRAWSEQ
    
    # fix rownames
    rownames(dat.inla) <- 1:nrow(dat.inla)
    
    # variables for INLA model
    
    dat.inla$year.month4 <- dat.inla$year.month3 <- dat.inla$year.month2 <- dat.inla$year.month
    dat.inla$month7 <- dat.inla$month6 <- dat.inla$month5 <- dat.inla$month4 <- dat.inla$month3 <- dat.inla$month2 <- dat.inla$month
    dat.inla$ID5 <- dat.inla$ID4 <- dat.inla$ID3 <- dat.inla$ID2 <- dat.inla$ID
    dat.inla$e <- 1:nrow(dat.inla)
    
    # INLA
    
    if(type==17){
        
        # 1. Type Id space-time interaction with besag state interaction terms and state-month specific variable slope (iid)
        # NO MONTH TERMS AND NO RW1
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        #f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        #f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        #f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        #f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable1, model="iid") +                                              # month specific climate slope
        f(month6, variable2, model="iid") +                                              # month specific climate slope
        f(month7, variable3, model="iid") +                                              # month specific climate slope
        # random walk across time
        #f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }

    if(type==16){

        # 1. Type Id space-time interaction with besag state interaction terms and state-month specific variable slope (iid)
        # NO MONTH TERMS
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        #f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        #f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        #f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        #f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable1, model="iid") +                                              # month specific climate slope
        f(month6, variable2, model="iid") +                                              # month specific climate slope
        f(month7, variable3, model="iid") +                                              # month specific climate slope
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }

    if(type==1){

        # 1. Type I space-time interaction
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                            		# global slope
        # month specific terms
        f(month, model="rw1",cyclic = TRUE) +                                   		# month specific intercept
        f(month2, year.month2, model="rw1",cyclic = TRUE) +                     		# month specific slope
        # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable1, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        f(month6, variable2, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        f(month7, variable3, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        #f(month5, variable, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific climate slope (spatially-correlated)
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                     		# overdispersion term
    }

    if(type==2){

        # 1. Type Ia space-time interaction
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable1, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        f(month6, variable2, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        f(month7, variable3, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        #f(month5, variable, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific climate slope (spatially-correlated)
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }

    if(type==3) {

        # 2. Type II space-time interaction
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                            		# global slope
        # month specific terms
        f(month, model="rw1",cyclic = TRUE) +                                   		# month specific intercept
        f(month2, year.month2, model="rw1",cyclic = TRUE) +                     		# month specific slope
        # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable1, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        f(month6, variable2, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        f(month7, variable3, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        #f(month5, variable, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific climate slope (spatially-correlated)
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # space-time interaction
        f(ID3,model="iid", group=year.month4,                                   		# type II space-time interaction
        control.group=list(model="rw1")) +
        #control.group=list(model="rw2")) +
        #control.group=list(model="exchangeable")) +
        # overdispersion term
        f(e, model = "iid")                                                     		# overdispersion term
    }

    if(type==4) {

        # 2. Type IIa space-time interaction
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                             # global intercept
        year.month +                                                                    # global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                              # state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                                # state specific slope (BYM)
        # climate specific terms
        f(month5, variable1, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        f(month6, variable2, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        f(month7, variable3, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        #f(month5, variable, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific climate slope (spatially-correlated)
        # random walk across time
        f(year.month3, model="rw1") +                                                   # rw1
        # space-time interaction
        f(ID3,model="iid", group=year.month4,                                           # type II space-time interaction
        control.group=list(model="rw1")) +
        #control.group=list(model="rw2")) +
        #control.group=list(model="exchangeable")) +
        # overdispersion term
        f(e, model = "iid")                                                             # overdispersion term
    }

    if(type==5) {

        # 3. Type III space-time interaction
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           		 	# global slope
        # month specific terms
        f(month, model="rw1",cyclic = TRUE) +                                   		# month specific intercept
        f(month2, year.month2, model="rw1",cyclic = TRUE) +                     		# month specific slope
        # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable1, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        f(month6, variable2, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        f(month7, variable3, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        #f(month5, variable, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific climate slope (spatially-correlated)
        # random walk terms
        f(year.month3, model="rw1") +                                           		# rw1
        #f(year.month4,model="iid", group=ID3,                                          # type III space-time interaction
        f(year.month4,model="rw1", group=ID3,                                           # variation on model
        control.group=list(model="I")) +
        #control.group=list(model="besag",                                              # variation on model
        #graph=USA.adj)) +                                                              # variation on model
        # overdispersion term
        f(e, model = "iid")                                                     		# overdispersion term
    }

    if(type==6) {

        # 3. Type IIIa space-time interaction
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           		 	# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable1, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        f(month6, variable2, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        f(month7, variable3, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        #f(month5, variable, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific climate slope (spatially-correlated)
        #f(year.month3, model="rw1") +                                           		# rw1 across time (put back?)
        #f(year.month4,model="iid", group=ID3,                                          # type III space-time interaction
        f(year.month4,model="rw1", group=ID3,                                           # variation on model
        control.group=list(model="I")) +
        #control.group=list(model="besag",                                              # variation on model
        #graph=USA.adj)) +                                                              # variation on model
        # overdispersion term
        f(e, model = "iid")                                                     		# overdispersion term
    }

    if(type==7) {

        # 4. Type IV space-time interaction

        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                            		# global slope
        # month specific terms
        f(month, model="rw1",cyclic = TRUE) +                                   		# month specific intercept
        f(month2, year.month2, model="rw1",cyclic = TRUE) +                     		# month specific slope
        # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                     		 	# state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable1, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        f(month6, variable2, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        f(month7, variable3, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        #f(month5, variable, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific climate slope (spatially-correlated)
        # random walk terms
        f(year.month3, model="rw1") +                                           		# rw1
        f(ID3,model="besag", graph=USA.adj,                                     		# type IV space-time interaction
        group=year.month4,
        control.group=list(model="rw1")) +
        # overdispersion term
        f(e, model = "iid")                                                     		# overdispersion term
    }

    if(type==8){

        # 1. Type Ib space-time interaction with besag state interaction terms and national variable slope
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        variable1 +                                                                      # national variable slope
        variable2 +                                                                      # national variable slope
        variable3 +                                                                      # national variable slope
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }

    if(type==9){

        # 1. Type Ic space-time interaction with besag state interaction terms and state specific variable slope
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(ID3, variable1, model='iid') +                                                 # state specific climate slope
        f(ID4, variable2, model='iid') +                                                 # state specific climate slope
        f(ID5, variable3, model='iid') +                                                 # state specific climate slope
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }

    if(type==10){

        # 1. Type Id space-time interaction with besag state interaction terms and state-month specific variable slope (rw1)
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable1, model="rw1", cyclic=TRUE) +                                              # month specific climate slope
        f(month6, variable2, model="rw1", cyclic=TRUE) +                                              # month specific climate slope
        f(month7, variable3, model="rw1", cyclic=TRUE) +                                              # month specific climate slope
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }

    if(type==11){

        # 1. Type Ie space-time interaction with besag state interaction terms and state-month specific variable slope (rw1)
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable1, model="rw1",cyclic = TRUE, group=ID) +                      # state-month specific climate slope
        f(month6, variable2, model="rw1",cyclic = TRUE, group=ID) +                      # state-month specific climate slope
        f(month7, variable3, model="rw1",cyclic = TRUE, group=ID) +                      # state-month specific climate slope
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }



    if(type==12){

        # 1. Type If space-time interaction with besag state interaction terms and spatially-correlated state-month specific variable slope (rw1)
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable1, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific climate slope (spatially-correlated)
        f(month6, variable2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific climate slope (spatially-correlated)
        f(month7, variable3, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific climate slope (spatially-correlated)
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }

    if(type==13){

        # 1. Type Ief space-time interaction with besag state interaction terms and spatially-correlated state-month specific variable slope (rw1)
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(ID3, variable, model='iid') +                                                 # state specific climate slope
        f(month5, variable1, model="iid") +                                              # month specific climate slope
        f(month6, variable2, model="iid") +                                              # month specific climate slope
        f(month7, variable3, model="iid") +                                              # month specific climate slope
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }

    if(type==14){

        # 1. Type Ide space-time interaction with besag state interaction terms and separate month- and state-specific variable slope (iid)
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(ID3, variable1, model='iid') +                                                 # state specific climate slope
        f(ID4, variable2, model='iid') +                                                 # state specific climate slope
        f(month5, variable1, model="iid") +                                              # month specific climate slope
        f(month6, variable2, model="iid") +                                              # month specific climate slope
        f(month7, variable3, model="iid") +                                              # month specific climate slope
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }

    if(type==15){

        # 1. Type Ig space-time interaction with besag state interaction terms and region-month specific variable slope (rw1)
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable1, model="rw1",cyclic = TRUE, group=ID.clim) +                 # state-month specific climate slope
        f(month6, variable2, model="rw1",cyclic = TRUE, group=ID.clim) +                 # state-month specific climate slope
        f(month7, variable3, model="rw1",cyclic = TRUE, group=ID.clim) +                 # state-month specific climate slope
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }
    
    # INLA model rough
    system.time(mod.rough <-
    inla(formula = fml,
    family = "poisson",
    data = dat.inla,
    E = pop.adj,
    control.compute = list(dic=TRUE),
    control.predictor = list(link = 1),
    control.inla = list(diagonal=10000, int.strategy='eb',strategy='gaussian'),
    #verbose=TRUE
    ))
    
    # INLA model proper
    system.time(mod <-
    inla(formula = fml,
    family = "poisson",
    data = dat.inla,
    E = pop.adj,
    control.compute = list(dic=TRUE),
    control.predictor = list(link = 1),
    control.inla=list(diagonal=0),
    control.mode = list(result = mod.rough, restart = TRUE),
    #verbose=TRUE
    ))
    
   # create directory for output
    file.loc <- paste0('~/data/mortality/US/state/climate_effects/',dname.arg,'/3var/',metric.arg,'/non_pw/type_',type.selected,'/age_groups/',age.sel)
    ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)
    
    if(cod.arg!='AllCause'){
        # save all parameters of INLA model
        parameters.name <- paste0('USA_rate_pred_type',type.selected,'_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end,'_',dname.arg,'_',metric.arg,'_',cause,'_parameters_fast')
        #mod$misc <- NULL
        #mod$.args$.parent.frame <- NULL
        if(cluster==0){saveRDS(mod,paste0(file.loc,'/',parameters.name))}
        if(cluster==1){saveRDS(mod,paste0('../output/pred/',parameters.name))}

        # save summary of INLA model
        summary.name <- paste0('USA_rate_pred_type',type.selected,'_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end,'_',dname.arg,'_',metric.arg,'_',cause,'_summary_fast.txt')
        inla.summary.mod <- summary(mod)
        if(cluster==0){capture.output(inla.summary.mod,file=paste0(file.loc,'/',summary.name))}
        if(cluster==1){capture.output(inla.summary.mod,file=paste0('../output/summary/',summary.name))}

        # capture output for emailing purposes
        email.content <- capture.output(inla.summary.mod)

        # save RDS of INLA results
        plot.dat <- as.data.frame(cbind(dat.inla,rate.pred=mod$summary.fitted.values$mean,sd=mod$summary.fitted.values$sd))

        # name of RDS output file then save
        RDS.name <- paste0('USA_rate_pred_type',type.selected,'_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end,'_',dname.arg,'_',metric.arg,'_',cause,'_fast')
        if(cluster==0){saveRDS(plot.dat,paste0(file.loc,'/',RDS.name))}
        if(cluster==1){saveRDS(plot.dat,paste0('../output/pred/',RDS.name))}
    }

    if(cod.arg =='AllCause'){
        # save all parameters of INLA model
        parameters.name <- paste0('USA_rate_pred_type',type.selected,'_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end,'_',dname.arg,'_',metric.arg,'_parameters_fast')
        #mod$misc <- NULL
        #mod$.args$.parent.frame <- NULL
        if(cluster==0){saveRDS(mod,paste0(file.loc,'/',parameters.name))}
        if(cluster==1){saveRDS(mod,paste0('../output/pred/',parameters.name))}

        # save summary of INLA model
        summary.name <- paste0('USA_rate_pred_type',type.selected,'_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end,'_',dname.arg,'_',metric.arg,'_summary_fast.txt')
        inla.summary.mod <- summary(mod)
        if(cluster==0){capture.output(inla.summary.mod,file=paste0(file.loc,'/',summary.name))}
        if(cluster==1){capture.output(inla.summary.mod,file=paste0('../output/summary/',summary.name))}

        # capture output for emailing purposes
        email.content <- capture.output(inla.summary.mod)

        # save RDS of INLA results
        plot.dat <- as.data.frame(cbind(dat.inla,rate.pred=mod$summary.fitted.values$mean,sd=mod$summary.fitted.values$sd))

        # name of RDS output file then save
        RDS.name <- paste0('USA_rate_pred_type',type.selected,'_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end,'_',dname.arg,'_',metric.arg,'_fast')
        if(cluster==0){saveRDS(plot.dat,paste0(file.loc,'/',RDS.name))}
        if(cluster==1){saveRDS(plot.dat,paste0('../output/pred/',RDS.name))}
    }

    sender <- "emailr349@gmail.com"
    recipients <- c("r.parks15@imperial.ac.uk")
    send.mail(from = sender,
    to = recipients,
    subject = paste0(sex.lookup[sex.sel],' ',age.sel,' model ',type.selected,' ',dname.arg,' ',metric.arg,'_',cause,' fast non-pw done'),
    body = "Well done",
    smtp = list(host.name = "smtp.gmail.com", port = 465,
    user.name = "emailr349@gmail.com",
    passwd = "inlaisthebest", ssl = TRUE),
    authenticate = TRUE,
    send = TRUE)
    
    # this bracket ends the function at the top of the script
}

# functions to enable age group and sex to be selected with faster AR1 structure in addition to rough run
inla.function.climate.faster <- function(age.sel,sex.sel,year.start,year.end,type,cluster,cause='Allcause') {
    
    dat.inla <- dat.merged
    
    # filter all data by sex age and month
    sex <- sex.sel
    age <- age.sel
    fit.years <- year.start:year.end
    dat.inla <- dat.inla[dat.inla$sex==sex & dat.inla$age==age & dat.inla$year %in% fit.years,]
    
    # load drawseq lookup
    drawseq.lookup <-readRDS('~/git/mortality/USA/state/output/adj_matrix_create/drawseq.lookup.rds')
    
    #dat.inla <- merge(dat.inla,drawseq.lookup, by='fips')
    
    # extract unique table of year and months to generate year.month
    dat.year.month <- unique(dat.inla[,c('year', 'month')])
    dat.year.month$month <- as.integer(dat.year.month$month)
    dat.year.month$year.month <- seq(nrow(dat.year.month))
    
    # merge year.month table with population table to create year.month id
    dat.inla <- merge(dat.inla,dat.year.month, by=c('year','month'))
    
    # make sure that the order of the main data file matches that of the shapefile,
    # otherwise the model will not be valid
    dat.inla <- dat.inla[order(dat.inla$DRAWSEQ,dat.inla$sex,dat.inla$age,dat.inla$year.month),]
    
    # add ID column for INLA
    dat.inla$ID <- dat.inla$DRAWSEQ
    
    # fix rownames
    rownames(dat.inla) <- 1:nrow(dat.inla)
    
    # variables for INLA model
    
    dat.inla$year.month4 <- dat.inla$year.month3 <- dat.inla$year.month2 <- dat.inla$year.month
    dat.inla$month7 <- dat.inla$month6 <- dat.inla$month5 <- dat.inla$month4 <- dat.inla$month3 <- dat.inla$month2 <- dat.inla$month
    dat.inla$ID5 <- dat.inla$ID4 <- dat.inla$ID3 <- dat.inla$ID2 <- dat.inla$ID
    dat.inla$e <- 1:nrow(dat.inla)
    
    # INLA
    
    if(type==17){
        
        # 1. Type Id space-time interaction with besag state interaction terms and state-month specific variable slope (iid)
        # NO MONTH TERMS AND NO RW1
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        #f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        #f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        #f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        #f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable, model="iid") +                                              # month specific climate slope
        # random walk across time
        #f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }
    
    if(type==16){
        
        # 1. Type Id space-time interaction with besag state interaction terms and state-month specific variable slope (iid)
        # NO MONTH TERMS
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        #f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        #f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        #f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        #f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable, model="iid") +                                              # month specific climate slope
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }
    
    if(type==1){
        
        # 1. Type I space-time interaction
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                            		# global slope
        # month specific terms
        f(month, model="rw1",cyclic = TRUE) +                                   		# month specific intercept
        f(month2, year.month2, model="rw1",cyclic = TRUE) +                     		# month specific slope
        # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        #f(month5, variable, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific climate slope (spatially-correlated)
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                     		# overdispersion term
    }
    
    if(type==2){
        
        # 1. Type Ia space-time interaction
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        #f(month5, variable, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific climate slope (spatially-correlated)
        # random walk across time
        f(year.month3, model="ar1",hyper=list(rho=list(initial=log((1+0.99)/(1-0.99)),fixed=TRUE))) +                # pseudo rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }
    
    if(type==3) {
        
        # 2. Type II space-time interaction
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                            		# global slope
        # month specific terms
        f(month, model="rw1",cyclic = TRUE) +                                   		# month specific intercept
        f(month2, year.month2, model="rw1",cyclic = TRUE) +                     		# month specific slope
        # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        #f(month5, variable, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific climate slope (spatially-correlated)
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # space-time interaction
        f(ID3,model="iid", group=year.month4,                                   		# type II space-time interaction
        control.group=list(model="ar1"),hyper=list(rho=list(initial=log((1+0.99)/(1-0.99)),fixed=TRUE))) +
        #control.group=list(model="rw2")) +
        #control.group=list(model="exchangeable")) +
        # overdispersion term
        f(e, model = "iid")                                                     		# overdispersion term
    }
    
    if(type==4) {
        
        # 2. Type IIa space-time interaction
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                             # global intercept
        year.month +                                                                    # global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                              # state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                                # state specific slope (BYM)
        # climate specific terms
        f(month5, variable, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        #f(month5, variable, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific climate slope (spatially-correlated)
        # random walk across time
        f(year.month3, model="rw1") +                                                   # rw1
        # space-time interaction
        f(ID3,model="iid", group=year.month4,                                           # type II space-time interaction
        control.group=list(model="rw1")) +
        #control.group=list(model="rw2")) +
        #control.group=list(model="exchangeable")) +
        # overdispersion term
        f(e, model = "iid")                                                             # overdispersion term
    }
    
    if(type==5) {
        
        # 3. Type III space-time interaction
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           		 	# global slope
        # month specific terms
        f(month, model="rw1",cyclic = TRUE) +                                   		# month specific intercept
        f(month2, year.month2, model="rw1",cyclic = TRUE) +                     		# month specific slope
        # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        #f(month5, variable, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific climate slope (spatially-correlated)
        # random walk terms
        f(year.month3, model="rw1") +                                           		# rw1
        #f(year.month4,model="iid", group=ID3,                                          # type III space-time interaction
        f(year.month4,model="rw1", group=ID3,                                           # variation on model
        control.group=list(model="I")) +
        #control.group=list(model="besag",                                              # variation on model
        #graph=USA.adj)) +                                                              # variation on model
        # overdispersion term
        f(e, model = "iid")                                                     		# overdispersion term
    }
    
    if(type==6) {
        
        # 3. Type IIIa space-time interaction
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           		 	# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        #f(month5, variable, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific climate slope (spatially-correlated)
        #f(year.month3, model="rw1") +                                           		# rw1 across time (put back?)
        #f(year.month4,model="iid", group=ID3,                                          # type III space-time interaction
        f(year.month4,model="rw1", group=ID3,                                           # variation on model
        control.group=list(model="I")) +
        #control.group=list(model="besag",                                              # variation on model
        #graph=USA.adj)) +                                                              # variation on model
        # overdispersion term
        f(e, model = "iid")                                                     		# overdispersion term
    }
    
    if(type==7) {
        
        # 4. Type IV space-time interaction
        
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                            		# global slope
        # month specific terms
        f(month, model="rw1",cyclic = TRUE) +                                   		# month specific intercept
        f(month2, year.month2, model="rw1",cyclic = TRUE) +                     		# month specific slope
        # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                     		 	# state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        #f(month5, variable, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific climate slope (spatially-correlated)
        # random walk terms
        f(year.month3, model="rw1") +                                           		# rw1
        f(ID3,model="besag", graph=USA.adj,                                     		# type IV space-time interaction
        group=year.month4,
        control.group=list(model="rw1")) +
        # overdispersion term
        f(e, model = "iid")                                                     		# overdispersion term
    }
    
    if(type==8){
        
        # 1. Type Ib space-time interaction with besag state interaction terms and national variable slope
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        variable +                                                                      # national variable slope
        # random walk across time
        f(year.month3, model="ar1",hyper=list(rho=list(initial=log((1+0.99)/(1-0.99)),fixed=TRUE))) +                # pseudo rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }
    
    if(type==9){
        
        # 1. Type Ic space-time interaction with besag state interaction terms and state specific variable slope
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(ID3, variable, model='iid') +                                                 # state specific climate slope
        # random walk across time
        f(year.month3, model="ar1",hyper=list(rho=list(initial=log((1+0.99)/(1-0.99)),fixed=TRUE))) +                # pseudo rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }
    
    if(type==10){
        
        # 1. Type Id space-time interaction with besag state interaction terms and state-month specific variable slope (rw1)
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable, model="rw1", cyclic=TRUE) +                                              # month specific climate slope
        # random walk across time
        f(year.month3, model="ar1",hyper=list(rho=list(initial=log((1+0.99)/(1-0.99)),fixed=TRUE))) +                # pseudo rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }
    
    if(type==11){
        
        # 1. Type Ie space-time interaction with besag state interaction terms and state-month specific variable slope (rw1)
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable, model="rw1",cyclic = TRUE, group=ID) +                      # state-month specific climate slope
        # random walk across time
        f(year.month3, model="ar1",hyper=list(rho=list(initial=log((1+0.99)/(1-0.99)),fixed=TRUE))) +                # pseudo rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }
    
    
    
    if(type==12){
        
        # 1. Type If space-time interaction with besag state interaction terms and spatially-correlated state-month specific variable slope (rw1)
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific climate slope (spatially-correlated)
        # random walk across time
        f(year.month3, model="ar1",hyper=list(rho=list(initial=log((1+0.99)/(1-0.99)),fixed=TRUE))) +                # pseudo rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }
    
    if(type==13){
        
        # 1. Type Ief space-time interaction with besag state interaction terms and spatially-correlated state-month specific variable slope (rw1)
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(ID3, variable, model='iid') +                                                 # state specific climate slope
        f(month5, variable, model="iid") +                                              # month specific climate slope
        # random walk across time
        f(year.month3, model="ar1",hyper=list(rho=list(initial=log((1+0.99)/(1-0.99)),fixed=TRUE))) +                # pseudo rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }
    
    if(type==14){
        
        # 1. Type Ide space-time interaction with besag state interaction terms and separate month- and state-specific variable slope (iid)
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(ID3, variable, model='iid') +                                                 # state specific climate slope
        f(month5, variable, model="iid") +                                              # month specific climate slope
        # random walk across time
        f(year.month3, model="ar1",hyper=list(rho=list(initial=log((1+0.99)/(1-0.99)),fixed=TRUE))) +                # pseudo rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }
    
    if(type==15){
        
        # 1. Type Ig space-time interaction with besag state interaction terms and region-month specific variable slope (rw1)
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable, model="rw1",cyclic = TRUE, group=ID.clim) +                 # state-month specific climate slope
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }
    
    # INLA model rough
    system.time(mod.rough <-
    inla(formula = fml,
    family = "poisson",
    data = dat.inla,
    E = pop.adj,
    control.compute = list(dic=TRUE),
    control.predictor = list(link = 1),
    control.inla = list(diagonal=10000, int.strategy='eb',strategy='gaussian'),
    #verbose=TRUE
    ))
    
    # INLA model proper
    system.time(mod <-
    inla(formula = fml,
    family = "poisson",
    data = dat.inla,
    E = pop.adj,
    control.compute = list(dic=TRUE),
    control.predictor = list(link = 1),
    control.inla=list(diagonal=0),
    control.mode = list(result = mod.rough, restart = TRUE),
    #verbose=TRUE
    ))
    
    # create directory for output
    file.loc <- paste0('~/data/mortality/US/state/climate_effects/',dname.arg,'/3var/',metric.arg,'/non_pw/type_',type.selected,'/age_groups/',age.sel)
    ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)
    
    if(cod.arg!='AllCause'){
        # save all parameters of INLA model
        parameters.name <- paste0('USA_rate_pred_type',type.selected,'_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end,'_',dname.arg,'_',metric.arg,'_',cause,'_parameters_faster') # CURRRENTLY HERE!!!
        #mod$misc <- NULL
        #mod$.args$.parent.frame <- NULL
        if(cluster==0){saveRDS(mod,paste0(file.loc,'/',parameters.name))}
        if(cluster==1){saveRDS(mod,paste0('../output/pred/',parameters.name))}

        # save summary of INLA model
        summary.name <- paste0('USA_rate_pred_type',type.selected,'_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end,'_',dname.arg,'_',metric.arg,'_',cause,'_summary_faster.txt')
        inla.summary.mod <- summary(mod)
        if(cluster==0){capture.output(inla.summary.mod,file=paste0(file.loc,'/',summary.name))}
        if(cluster==1){capture.output(inla.summary.mod,file=paste0('../output/summary/',summary.name))}

        # capture output for emailing purposes
        email.content <- capture.output(inla.summary.mod)

        # save RDS of INLA results
        plot.dat <- as.data.frame(cbind(dat.inla,rate.pred=mod$summary.fitted.values$mean,sd=mod$summary.fitted.values$sd))

        # name of RDS output file then save
        RDS.name <- paste0('USA_rate_pred_type',type.selected,'_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end,'_',dname.arg,'_',metric.arg,'_',cause,'_faster')
        if(cluster==0){saveRDS(plot.dat,paste0(file.loc,'/',RDS.name))}
        if(cluster==1){saveRDS(plot.dat,paste0('../output/pred/',RDS.name))}
    }

    if(cod.arg =='AllCause'){
        # save all parameters of INLA model
        parameters.name <- paste0('USA_rate_pred_type',type.selected,'_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end,'_',dname.arg,'_',metric.arg,'_parameters_faster')
        #mod$misc <- NULL
        #mod$.args$.parent.frame <- NULL
        if(cluster==0){saveRDS(mod,paste0(file.loc,'/',parameters.name))}
        if(cluster==1){saveRDS(mod,paste0('../output/pred/',parameters.name))}

        # save summary of INLA model
        summary.name <- paste0('USA_rate_pred_type',type.selected,'_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end,'_',dname.arg,'_',metric.arg,'_summary_faster.txt')
        inla.summary.mod <- summary(mod)
        if(cluster==0){capture.output(inla.summary.mod,file=paste0(file.loc,'/',summary.name))}
        if(cluster==1){capture.output(inla.summary.mod,file=paste0('../output/summary/',summary.name))}

        # capture output for emailing purposes
        email.content <- capture.output(inla.summary.mod)

        # save RDS of INLA results
        plot.dat <- as.data.frame(cbind(dat.inla,rate.pred=mod$summary.fitted.values$mean,sd=mod$summary.fitted.values$sd))

        # name of RDS output file then save
        RDS.name <- paste0('USA_rate_pred_type',type.selected,'_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end,'_',dname.arg,'_',metric.arg,'_faster')
        if(cluster==0){saveRDS(plot.dat,paste0(file.loc,'/',RDS.name))}
        if(cluster==1){saveRDS(plot.dat,paste0('../output/pred/',RDS.name))}
    }
    
    sender <- "emailr349@gmail.com"
    recipients <- c("r.parks15@imperial.ac.uk")
    send.mail(from = sender,
    to = recipients,
    subject = paste0(sex.lookup[sex.sel],' ',age.sel,' model ',type.selected,' ',dname.arg,' ',metric.arg,'_',cause,' faster non-pw done'),
    body = "Well done",
    smtp = list(host.name = "smtp.gmail.com", port = 465,
    user.name = "emailr349@gmail.com",
    passwd = "inlaisthebest", ssl = TRUE),
    authenticate = TRUE,
    send = TRUE)
    
    # this bracket ends the function at the top of the script
}