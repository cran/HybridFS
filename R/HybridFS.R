#' A Hybrid Feature Selection Function
#'
#' HybridFS is a combination of filter and wrapper methods which uses a set of statistical tests for feature selection. Primary level feature reduction involves filtering based on statistical test such as Chi-Square test of Independence, Information value(IV) and Entropy-related methods. Features filtered at this level are further fed into a classification algorithm and final features of the optimal model is returned along with the feature importance.
#' @param input.df Input data frame that contains the target variable and predictor variables with no missing values. Predictors can be either categorical or continuous.Unique identifier,if present should be named "ID".
#' @param target.var.name Name of binary target variable. Target variables should be integer with only two distinct values (0, 1)
#' @return An object of class FS, which is a list with the following components:
#' @return \item{imp.features}{A data frame of the selected features from the optimal model retuned with the relative rank.Variable importance plot for top 10 variables selected is displayed.Continuous features selected are returned as binned variables (e.g. average_volume is returned as average_volume.binned)}
#' @details
#' \emph{Binning of Continuous Predictors}\cr
#' Supervised Binning of continuous predictors reduces computational time, improves model performance and predictive power. Binning is implemented based on similar weight of evidence (WOE) values and information value (IV). Transformed dataset with binned copy of continuous variables is then fed into the Hybrid filter-Wrapper algorithm. Continuous features selected are returned as binned variables (e.g. average_volume is returned as average_volume.binned). To retrieve the transformed dataset, use FinalBinnedData() function.\cr
#'
#' \emph{Level1 Feature Reduction - Filter Method}\cr
#' Chi-Square test of Independence, Information value(IV) and Entropy-related methods such as Information Gain, Gain Ratio and Symmetrical Uncertainty are used to generate variable importance scores. Top n features are dynamically selected and different subsets are formed based on relative ranking from each of the filter methods.\cr
#'
#' \emph{Level2 Feature Reduction - Wrapper Method}\cr
#' Different subsets of variables from the first level are trained using a classification algorithm. Optimum probability cut-off for the target class is determined by the K-S Statistic. Combination of Area Under the Curve(AUC) and F-score (F1 score) are used as the benchmark metrics to measure the model performance. Best set of features with variable importance and rank from the optimal model is returned. Out-of-Sample Validation results are also displayed to understand the stability of the optimal model selected.
#' @return \item{model.perf}{Performance metrics of the optimal model such as F1 Score, Accuracy, Precision and Recall are returned}
#' @export
#' @examples
#' FS=HybridFS(input.df=validation,target.var.name="Survived")
#' @note
#' Requires latest version of Java(8 and above)


HybridFS <- function(input.df, target.var.name){
  data<- as.data.frame(input.df)

  names(data)[names(data)==target.var.name] <- "DV"
  options(java.parameters = "-Xmx1g")
  data$DV<- as.integer(data$DV)
  options(warn=-1)

  # Requires Java version of 8 or greater


  ###########################
  ###### Data Binning #######
  ###########################

  dataBinning <- function(data){


    #subset all integer variables in dataset
    allIntVarDF <- data[,sapply(data,is.integer)]

    #Int variables with levels less than 12
    intVarsLen <- apply(allIntVarDF,2,function(i) length(unique(i))<=12)
    intvar<-names(intVarsLen)

    #Int variables with more than 12 levels
    intbin_var <- allIntVarDF[,names(intVarsLen[intVarsLen==FALSE])]
    intbin_var2<- names(intbin_var)
    numvars <- names(data[,sapply(data,is.numeric)])
    numbin_var<-setdiff(numvars,intvar)


    #Supervised Binning of variables based of woe
    binning <- woeBinning::woe.binning(data, 'DV', c(numbin_var,intbin_var2))
    tabulate.binning <- woeBinning::woe.binning.table(binning)
    #tabulate.binning

    #Adding binned variables to dataset
    data_binned <- woeBinning::woe.binning.deploy(data, binning)

    #Checking for all factor variables in the new dataset
    #catvar<-names(data_binned[,sapply(data_binned,is.factor)])
    #nrow(catvar)
    return(data_binned)
  }

  ######################################
  ###### Univariate filter Tests #######
  ######################################
  univFiltRes <- function (data_binned){



    allCatVarDF <- data_binned[,sapply(data_binned,is.factor),drop=FALSE]
    allIntVarDF <- data_binned[,sapply(data_binned,is.integer),drop=FALSE]
    allChrVarDF <- data_binned[,sapply(data_binned,is.character),drop=FALSE]

    # Remove dependent variable and get all integer variables which have less than 12 unique values,
    # as these can converted to factor directly
    allIntVarDF$DV <- NULL
    intVarsLen <- apply(allIntVarDF,2,function(i) length(unique(i))<=12)
    allIntVarDF <- allIntVarDF[,names(intVarsLen[intVarsLen==TRUE])]

    #chrVarsLen <- apply(allChrVarDF,2,function(i) length(unique(i))<=12)
    #allChrVarDF <- allChrVarDF[,names(chrVarsLen[chrVarsLen==TRUE])]


    allCatVarFinalDF <- data_binned[,c("DV",names(allIntVarDF),names(allCatVarDF),names(allChrVarDF))]
    allCatVarFinalDF[,names(allIntVarDF)] <- lapply(allCatVarFinalDF[,names(allIntVarDF)],factor)
    allCatVarFinalDF[,names(allChrVarDF)] <- lapply(allCatVarFinalDF[,names(allChrVarDF)],factor)
    allCatVarFinalDF$ID.binned <- NULL

    # Remove factor variables with more than 50 levels
    catVarsLen <- apply(allCatVarFinalDF,2,function(i) length(unique(i))>50)
    allCatVarFinalDF <- allCatVarFinalDF[,names(catVarsLen[catVarsLen==FALSE])]

    # Chi-sq Test
    chisqallpvalues <- apply(allCatVarFinalDF[-1] , 2 , function(i) stats::chisq.test(table(allCatVarFinalDF$DV , i ))$p.value)
    chisqallstatvals <- apply(allCatVarFinalDF[-1] , 2 , function(i) stats::chisq.test(table(allCatVarFinalDF$DV , i ))$statistic)
    chisq <- data.frame(VARS=names(chisqallpvalues),pval=chisqallpvalues,chistat=chisqallstatvals,stringsAsFactors = F)
    row.names(chisq) <- NULL

    # Remove variables which have p-value of 0 from the chi-square test results
    #removeVars <- chisq[which(chisq$pval == 0),"VARS"]
    #allCatVarFinalDF <- allCatVarFinalDF[,!(names(allCatVarFinalDF) %in% removeVars)]
    #chisq <- chisq[-which(chisq$VARS == removeVars),]

    chisq <- chisq[order(-chisq$chistat),]
    chisq$ChistatRank <- order(-chisq$chistat)

    # Entropy Related Tests
    infGainAllVarsTest <- FSelector::information.gain(DV~.,allCatVarFinalDF)
    gainRatioAllVarsTest <- FSelector::gain.ratio(DV~.,allCatVarFinalDF)
    symUncAllVarsTest <- FSelector::symmetrical.uncertainty(DV~.,allCatVarFinalDF)

    # Prepare the data frame and add rank according to the importance values from each test
    entropy <- data.frame(VARS = row.names(infGainAllVarsTest),InfGain = infGainAllVarsTest$attr_importance,GainRatio = gainRatioAllVarsTest$attr_importance,SymUnc = symUncAllVarsTest$attr_importance,stringsAsFactors = F)
    entropy <- entropy[order(-entropy$InfGain),]
    entropy$InfGainRank <- order(-entropy$InfGain)
    entropy <- entropy[order(-entropy$GainRatio),]
    entropy$GainRatioRank <- order(-entropy$GainRatio)
    entropy <- entropy[order(-entropy$SymUnc),]
    entropy$SymUncRank <- order(-entropy$SymUnc)

    # Information Value Test
    factor_vars <-  names(allCatVarFinalDF[-1])
    all_iv <- data.frame(VARS=factor_vars, IV=numeric(length(factor_vars)), STRENGTH=character(length(factor_vars)), stringsAsFactors = F)  # init output dataframe
    for (factor_var in factor_vars)
    {
      all_iv[all_iv$VARS == factor_var, "IV"] <- InformationValue::IV(X=allCatVarFinalDF[, factor_var], Y=allCatVarFinalDF$DV)
      all_iv[all_iv$VARS == factor_var, "STRENGTH"] <- attr(InformationValue::IV(X=allCatVarFinalDF[, factor_var], Y=allCatVarFinalDF$DV), "howgood")
    }
    all_iv <- all_iv[order(-all_iv$IV),]
    all_iv$IVRank <- order(-all_iv$IV)

    # Putting all the results in a single dataframe
    allFiltRes <- merge(chisq,entropy,by="VARS")
    allFiltRes <- merge(allFiltRes,all_iv,by="VARS")

    # Create flags to check if the variable passed each of the five tests
    allFiltRes1 <- allFiltRes
    topN <- sum(allFiltRes1$IV >= 0.03)
    allFiltRes1$ChisqFLAG=ifelse(allFiltRes1$ChistatRank<=topN & allFiltRes1$pval<=0.05,1,0)
    allFiltRes1$IVFLAG=ifelse(allFiltRes1$ChistatRank<=topN,1,0)
    allFiltRes1$InfGainFLAG=ifelse(allFiltRes1$InfGainRank<=topN,1,0)
    allFiltRes1$GainRatioFLAG=ifelse(allFiltRes1$GainRatioRank<=topN,1,0)
    allFiltRes1$SymUncFLAG=ifelse(allFiltRes1$SymUncRank<=topN,1,0)

    #
    flags <- c("ChisqFLAG","IVFLAG","InfGainFLAG","GainRatioFLAG","SymUncFLAG")
    allFiltRes1$Cleared <- apply(allFiltRes1[,flags],1,sum)
    #write.csv(allFiltRes1,"allFilterResults.csv")
    return(allFiltRes1)
  }


  ######################
  #######Block-5########
  ######################


  LogisticResults = function(data_binned){

    ##5-cleared
    mdata = subset(data_binned ,select = c("DV",allFiltRes[allFiltRes$Cleared==5,"VARS"]))

    ##Break Train test (binned vars)
    set.seed(100)
    sfac=caTools::sample.split(mdata$DV, SplitRatio = 0.7)
    train=subset(mdata, sfac==TRUE)
    test=subset(mdata, sfac==FALSE)
    #test=subset(test, !incorp_cntry=='United Stes')

    ##Model
    model1=stats::glm(as.factor(DV)~.,data = train,family = stats::binomial)
    equation = summary(model1)

    ##predictions
    predtrain=stats::predict(model1, newdata=train,type='response')
    predtest=stats::predict(model1, newdata=test,type='response')

    train1=train
    train1$pred=predtrain
    kstrain5=subset(train1, select = c('DV','pred'))
    kstrain5=kstrain5[order(-kstrain5$pred),]
    kstrain5$row=seq(1,nrow(kstrain5),1)

    a=as.numeric(round((nrow(train1)/10),0))
    a

    kstrain5$flag=ifelse(kstrain5$row<=(a),1,
                         ifelse(kstrain5$row>(a)&kstrain5$row<=(2*a),2,
                                ifelse(kstrain5$row>(2*a)&kstrain5$row<=(3*a),3,
                                       ifelse(kstrain5$row>(3*a)&kstrain5$row<=(4*a),4,
                                              ifelse(kstrain5$row>(4*a)&kstrain5$row<=(5*a),5,
                                                     ifelse(kstrain5$row>(5*a)&kstrain5$row<=(6*a),6,
                                                            ifelse(kstrain5$row>(6*a)&kstrain5$row<=(7*a),7,
                                                                   ifelse(kstrain5$row>(7*a)&kstrain5$row<=(8*a),8,
                                                                          ifelse(kstrain5$row>(8*a)&kstrain5$row<=(9*a),9,
                                                                                 ifelse(kstrain5$row>(9*a),10,0))))))))))

    Responders=0

    for(i in 1:10)
    {

      Responders[i] = sum(kstrain5$DV[kstrain5$flag==i])
    }

    kstat=data.frame(Responders)

    ##Variables
    for(i in 1:10)
    {
      kstat$Non_Responders[i]=(sum(kstrain5$flag==i)-kstat$Responders[i])
    }

    kstat$Responders_Percentage=((kstat$Responders)/sum(kstat$Responders))*100

    kstat$Non_Responders_Percentage=((kstat$Non_Responders)/sum(kstat$Non_Responders))*100

    #sum(kstrain5$DV[kstrain5$flag=='decile1'])
    ##R-Cumm
    kstat$R_Cumm[10]=kstat$Responders_Percentage[nrow(kstat)]

    num=c(9,8,7,6,5,4,3,2,1)

    for(i in num)
    {
      kstat$R_Cumm[i]=kstat$R_Cumm[i+1]+kstat$Responders_Percentage[i]
    }

    ##NR-Cumm
    kstat$NR_Cumm[10]=kstat$Non_Responders_Percentage[nrow(kstat)]

    num=c(9,8,7,6,5,4,3,2,1)

    for(i in num)
    {
      kstat$NR_Cumm[i]=kstat$NR_Cumm[i+1]+kstat$Non_Responders_Percentage[i]
    }

    kstat$KS=round((kstat$NR_Cumm-kstat$R_Cumm),2)

    num1=c(1,2,3,4,5,6,7,8,9,10)


    ##Decile determination
    for(i in num1)
    {
      if(kstat$KS[i+1]>kstat$KS[i])
      {count=i+1}
      else
      {
        #print(count)
        break
      }}

    thresh=round(kstrain5$pred[count*a],20)
    thresh

    con=table(train$DV,predtrain>=thresh)
    con=as.matrix(con)

    con1=table(test$DV,predtest>=thresh)
    con1=as.matrix(con1)

    tn  = con[1,1]
    fp  = con[1,2]
    fn  = con[2,1]
    tp  = con[2,2]
    precision = as.numeric((tp)/(tp+fp))
    recall    = as.numeric((tp)/(tp+fn))
    f1train = ((2*precision*recall)/(precision+recall))

    tn1  = con1[1,1]
    fp1  = con1[1,2]
    fn1  = con1[2,1]
    tp1  = con1[2,2]
    precision1 = as.numeric((tp1)/(tp1+fp1))
    recall1    = as.numeric((tp1)/(tp1+fn1))
    f1test = ((2*precision1*recall1)/(precision1+recall1))

    ##AUC
    ROCR1=ROCR::prediction(predtrain,train$DV)
    auctrain = as.numeric(ROCR::performance(ROCR1, 'auc')@y.values)
    ROCR2=ROCR::prediction(predtest,test$DV)
    auctest = as.numeric(ROCR::performance(ROCR2, "auc")@y.values)

    #Results <- list(f1test,f1train,precision,precision1,recall,recall1,auctrain,auctest)
    Results = as.data.frame(cbind(f1test,f1train,precision,precision1,recall,recall1,auctrain,auctest))
    colnames(Results) = c("F1_Test", "F1_Train","Precision_Train","Precision_Test","Recall_Train","Recall_Test","AUC_Train","AUC_Test")

    return(Results)
  }


  ######################
  ######Block-4&5#######
  ######################


  LogisticResults1 = function(data_binned){

    ##5-4-cleared
    mdata = subset(data_binned ,select = c("DV",allFiltRes[allFiltRes$Cleared>3,"VARS"]))

    ##Break Train test (binned vars)
    set.seed(100)
    sfac=caTools::sample.split(mdata$DV, SplitRatio = 0.7)
    train=subset(mdata, sfac==TRUE)
    test=subset(mdata, sfac==FALSE)
    #test=subset(test, !incorp_cntry=='United Stes')

    ##Model
    model1=stats::glm(DV~.,data = train,family = stats::binomial)
    equation = summary(model1)

    ##predictions
    predtrain=stats::predict(model1, newdata=train,type='response')
    predtest=stats::predict(model1, newdata=test,type='response')

    train1=train
    train1$pred=predtrain
    kstrain5=subset(train1, select = c('DV','pred'))
    kstrain5=kstrain5[order(-kstrain5$pred),]
    kstrain5$row=seq(1,nrow(kstrain5),1)

    a=as.numeric(round((nrow(train1)/10),0))
    a

    kstrain5$flag=ifelse(kstrain5$row<=(a),1,
                         ifelse(kstrain5$row>(a)&kstrain5$row<=(2*a),2,
                                ifelse(kstrain5$row>(2*a)&kstrain5$row<=(3*a),3,
                                       ifelse(kstrain5$row>(3*a)&kstrain5$row<=(4*a),4,
                                              ifelse(kstrain5$row>(4*a)&kstrain5$row<=(5*a),5,
                                                     ifelse(kstrain5$row>(5*a)&kstrain5$row<=(6*a),6,
                                                            ifelse(kstrain5$row>(6*a)&kstrain5$row<=(7*a),7,
                                                                   ifelse(kstrain5$row>(7*a)&kstrain5$row<=(8*a),8,
                                                                          ifelse(kstrain5$row>(8*a)&kstrain5$row<=(9*a),9,
                                                                                 ifelse(kstrain5$row>(9*a),10,0))))))))))

    Responders=0

    for(i in 1:10)
    {

      Responders[i] = sum(kstrain5$DV[kstrain5$flag==i])
    }

    kstat=data.frame(Responders)

    ##Variables
    for(i in 1:10)
    {
      kstat$Non_Responders[i]=(sum(kstrain5$flag==i)-kstat$Responders[i])
    }

    kstat$Responders_Percentage=((kstat$Responders)/sum(kstat$Responders))*100

    kstat$Non_Responders_Percentage=((kstat$Non_Responders)/sum(kstat$Non_Responders))*100

    #sum(kstrain5$DV[kstrain5$flag=='decile1'])
    ##R-Cumm
    kstat$R_Cumm[10]=kstat$Responders_Percentage[nrow(kstat)]

    num=c(9,8,7,6,5,4,3,2,1)

    for(i in num)
    {
      kstat$R_Cumm[i]=kstat$R_Cumm[i+1]+kstat$Responders_Percentage[i]
    }

    ##NR-Cumm
    kstat$NR_Cumm[10]=kstat$Non_Responders_Percentage[nrow(kstat)]

    num=c(9,8,7,6,5,4,3,2,1)

    for(i in num)
    {
      kstat$NR_Cumm[i]=kstat$NR_Cumm[i+1]+kstat$Non_Responders_Percentage[i]
    }

    kstat$KS=round((kstat$NR_Cumm-kstat$R_Cumm),2)

    num1=c(1,2,3,4,5,6,7,8,9,10)


    ##Decile determination
    for(i in num1)
    {
      if(kstat$KS[i+1]>kstat$KS[i])
      {count=i+1}
      else
      {
        #print(count)
        break
      }}

    thresh=round(kstrain5$pred[count*a],20)
    #thresh

    con=table(train$DV,predtrain>=thresh)
    con=as.matrix(con)

    con1=table(test$DV,predtest>=thresh)
    con1=as.matrix(con1)

    tn  = con[1,1]
    fp  = con[1,2]
    fn  = con[2,1]
    tp  = con[2,2]
    precision = as.numeric((tp)/(tp+fp))
    recall    = as.numeric((tp)/(tp+fn))
    f1train = ((2*precision*recall)/(precision+recall))

    tn1  = con1[1,1]
    fp1  = con1[1,2]
    fn1  = con1[2,1]
    tp1  = con1[2,2]
    precision1 = as.numeric((tp1)/(tp1+fp1))
    recall1    = as.numeric((tp1)/(tp1+fn1))
    f1test = ((2*precision1*recall1)/(precision1+recall1))

    ##AUC
    ROCR1=ROCR::prediction(predtrain,train$DV)
    auctrain = as.numeric(ROCR::performance(ROCR1, 'auc')@y.values)
    ROCR2=ROCR::prediction(predtest,test$DV)
    auctest = as.numeric(ROCR::performance(ROCR2, "auc")@y.values)

    #Results <- list(f1test,f1train,precision,precision1,recall,recall1,auctrain,auctest)
    Results = as.data.frame(cbind(f1test,f1train,precision,precision1,recall,recall1,auctrain,auctest))
    colnames(Results) = c("F1_Test", "F1_Train","Precision_Train","Precision_Test","Recall_Train","Recall_Test","AUC_Train","AUC_Test")

    return(Results)
  }



  ##RESULTS

  data_binned <- dataBinning(data)


  allFiltRes <- univFiltRes(data_binned)

  Results_4and5 = LogisticResults1(data_binned)

  Results_5 = LogisticResults(data_binned)

  Var_set_1 = list(allFiltRes$VARS[allFiltRes$Cleared>3])

  Var_set_2 = list(allFiltRes$VARS[allFiltRes$Cleared==5])

  ##Final set of variables (Selected features)
  Final_variable = ifelse((Results_4and5$F1_Test>Results_5$F1_Test),
                          Var_set_1,
                          ifelse((Results_4and5$F1_Test<Results_5$F1_Test),
                                 Var_set_2,
                                 ifelse(((Results_4and5$F1_Test=Results_5$F1_Test)&(Results_4and5$AUC_Test>Results_5$AUC_Test)),
                                        Var_set_1,Var_set_2)))
{
    if(identical(Final_variable,Var_set_1)==TRUE)
    {
      Final_Results = Results_4and5
    }
    else if(identical(Final_variable,Var_set_2)==TRUE)
    {
      Final_Results = Results_5
    }
}

  model.perf = as.data.frame(Final_Results)
  ##Replace dummy with the Final_results
  df1 = Final_Results[,c(2,3,5,7)]
  df2 = Final_Results[,c(1,4,6,8)]
  names(df1)[1:4] = c("F1Score","Precision","Recall","AUC")
  names(df2)[1:4] = c("F1Score","Precision","Recall","AUC")
  df1 = (t(df1))
  df1 = as.data.frame(df1)
  df1$Metrics = rownames(df1)
  names(df1)[1] = c("Train")
  df2 = (t(df2))
  df2 = as.data.frame(df2)
  df2$Metrics = rownames(df2)
  names(df2)[1] = c("Test")
  model.perf = merge(df1,df2,by = "Metrics")

  Final_variable = as.data.frame(Final_variable)
  colnames(Final_variable) = c("VARS")

  z = merge(Final_variable,allFiltRes[,c("VARS","chistat")],by = "VARS")
  z = z[order(-z$chistat),]

  z$Rank = seq(1,nrow(z),1)


  z$VariableImportance<-z$chistat
  z$chistat = NULL

  imp.features=z
  options(warn=0)
  FS <- list(imp.features,model.perf)



}


