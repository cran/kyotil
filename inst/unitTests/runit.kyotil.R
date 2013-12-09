### --- Test setup ---

if(FALSE) {
  library("RUnit")
  library("chngpt")
}

test.chngptm <- function() {

tolerance=1e-3

# more stringent tolerance for one system to ensure algorithm accuracy
if (R.Version()$system %in% c("x86_64, mingw32")) {
    tolerance=1e-6
}
 
RNGkind("Mersenne-Twister", "Inversion")





}
