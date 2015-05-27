# R Code
# DCoL Library
# Ho and Basu 2002; Ho et al. 2006; and Orriols-Puig et al. 2010
# A set of measures designed to characterize the apparent complexity of data sets

require(foreign);

cx.name = function(data) {

	name = paste(".", paste(sample(letters, 20, replace=TRUE), collapse=""), sep="");
	write.arff(data, paste(getwd(), "/", name, ".arff", sep=""));
	return(name);
}


cx.execute = function(name) {

	exe = paste(getwd(), "/./dcol -i ", getwd(), "/", name, ".arff -o ", getwd(), "/", name, " -F 1 -F 1v -F 2 -F 3 -F 4 -L 1 -L 2 -L 3 -N 1 -N 2 -N 3 -N 4 -T 1", sep="");
	system(paste(exe, sep=""), ignore.stdout=TRUE, ignore.stderr=TRUE);
	vet = read.table(paste(getwd(), "/", name, ".txt", sep=""), skip=17)[,-1];
	names(vet) = c("F1", "F1v", "F2", "F3", "F4", "L1", "L2", "L3", "N1", "N2", "N3", "N4", "T1");
	return(vet);	
}


cx.replace = function(name) {

	exe = paste(getwd(), "/./dcol -i ", getwd(), "/", name, ".arff -o ", getwd(), "/", name, " -F 1v -L 1 -L 2 -L 3 -d", sep="");
	system(paste(exe, sep=""), ignore.stdout=TRUE, ignore.stderr=TRUE);
	vet = colMeans(read.table(paste(getwd(), "/", name, ".txt", sep=""), skip=8)[,-1]);
	names(vet) = c("F1v", "L1", "L2", "L3");
	return(vet);
}


cx.remove = function(name) {
	system(paste("rm ", getwd(), "/", name, ".* ", sep=""));
}


complexity = function(data) {

	name = cx.name(data);
	dcol = cx.execute(name);

	if(nlevels(data$Class) > 2)
		dcol[,c("F1v", "L1", "L2", "L3")] = cx.replace(name);

	cx.remove(name);
	return(dcol);
}

