################################################################################################
################################################################################################

# Methods parameters:
# 	- pred: predictions from a classifier
#	- test: test examples from testing fold

################################################################################################
################################################################################################

#Cohen-Kappa measure
acc.kappa = function(pred, test) {

	levels(pred) = levels(test);
	aux = table(pred, test);
	pc = sum(apply(aux, 1, sum)/sum(aux) * apply(aux, 2, sum)/sum(aux));
	if(pc == 1){
		pc = 0;
	}
	aux = (sum(diag(aux))/sum(aux) - pc)/(1 - pc);
	return(aux);
}

################################################################################################
################################################################################################
#binary - recall

acc.bin.recall = function(pred, test){
browser();
	levels(pred) = levels(test);
	aux = table(pred, test);
	r = aux[1,1] / sum(aux[,1]);
	return(r);
}

################################################################################################
################################################################################################

acc.bin.precision = function(pred, test){

	levels(pred) = levels(test);
	aux = table(pred, test);
	p = aux[1,1] / sum(aux[1,])
	return(p);
}

################################################################################################
################################################################################################
acc.bin.measures = function(pred, test){

	obj = NULL;
	precision = acc.bin.precision(pred, test);
	recall = acc.bin.recall(pred, test);

	f = 2 * ((precision * recall) / (precision + recall));
	obj$precision = precision;
	obj$recall = recall;
	obj$fscore = f;

	return(obj);
}

################################################################################################
################################################################################################
#Area under ROC Curve measure
acc.auc = function(pred, test) {
	auc_accuracy = auc(roc(test, pred));
 	return(auc_accuracy);
}

################################################################################################
################################################################################################
# Simple Accuracy
acc.simple = function(pred, test) {
	levels(pred) = levels(test);
	t = table(pred, test)
	acc_simple = sum(diag(t))/sum(t);
	return(acc_simple);
}

################################################################################################
################################################################################################
#Evaluate a confusion matrix - problemas multiclasse
#	- (error, precision, recall, fscore)

acc.multi.measures = function(pred, test) {

	levels(pred) = levels(test);
	confusion.matrix = table(pred, test)
	final.matrix = vector("numeric", 4)
	names(final.matrix) = c("error", "precision", "recall", "fscore")
	
	mat.res = matrix(0, nrow(confusion.matrix), 3)
	rownames(mat.res) = colnames(confusion.matrix)
	colnames(mat.res) = c("precision", "recall", "fscore")
	
	for (i in 1:nrow(mat.res)){
	
		if (sum(confusion.matrix[,i])==0){
			mat.res[i,"precision"] = 0
		}else{
			mat.res[i,"precision"] = confusion.matrix[i,i]/sum(confusion.matrix[,i])
		}

		if (sum(confusion.matrix[i,])==0){
			mat.res[i,"recall"] = 0
		}else{
			mat.res[i,"recall"] = confusion.matrix[i,i]/sum(confusion.matrix[i,]) 
		}
		
		if ((mat.res[i,"precision"]==0) && (mat.res[i,"recall"]==0)){
			mat.res[i, "fscore"] = 0
		}else{
			mat.res[i,"fscore"] = (2*mat.res[i,"precision"]*mat.res[i,"recall"])/(mat.res[i,"precision"] + mat.res[i,"recall"])
		}		
	}
	
	idx = matrix(seq(1:nrow(confusion.matrix)),nrow(confusion.matrix),2)
	final.matrix["error"] = 1 - (sum(confusion.matrix[idx])/sum(confusion.matrix))
	final.matrix[c("precision", "recall", "fscore")] = apply(mat.res, 2, mean)
  final.matrix["accuracy"] <- 1 - final.matrix["error"]
  
	return (final.matrix);

}

################################################################################################
################################################################################################