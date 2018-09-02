## Hilliness Statistic ##

## The test is more or less considering how good the function is at climbing the hill of your matrix. 
## If it is significantly better at climbing the hill enough to get to the top of your actual matrix 
## much more often than permuted versions of your matrix, then your matrix is probably significantly hillier 
## than any random combination of the numbers that make up the matrix. Note this means your comparison 
## class is systems with the same kinds of values for entries in the matrix, if that is not the case 
## you can probably make a bunch of permuted matrices that are designed to act according to your null hypothesis.

## My impression is it needs a lot of resamplings to converge to a p-value, hence I set some defaults.

## A basic hill climbing algorithm

KingOfTheHill <- function(matrix, GradualSlope = F) { ## Doesn't work for 2x2 or smaller matrices
  RowIndices <- 1:nrow(matrix)
  ColIndices <- 1:ncol(matrix)
  Row <- sample(RowIndices, 1)
  Col <- sample(ColIndices, 1)
  if (isFALSE(GradualSlope)) {
    G <- 1
  }
  else { G <- max(2, nrow(matrix)%/%5, ncol(matrix)%/%5) }
  i <- 1
  NewRow <- Row
  NewCol <- Col
  while (i < length(matrix)) {
    i <- i+1
    max <- matrix[Row, Col]
    j <- sample(-G:G, 1)
    k <- sample(-G:G, 1)
    if (is.element(Row+j, RowIndices) & is.element(Col+k, ColIndices)) {
      if (matrix[Row+j, Col+k] > max) {
        max <- matrix[Row+j, Col+k]
        NewRow <- Row+j
        NewCol <- Col+k
      }
    }
    Row <- NewRow
    Col <- NewCol
    if (matrix[Row, Col] == max(matrix)) {
      return(matrix[Row, Col])
    }
  }
  return(matrix[Row, Col])
}

## Permutes the matrix structure

PermuteMatrix <- function(matrix) {
  Matrix <- matrix(sample(matrix), nrow = nrow(matrix))
}

## Function to be replicated

RandomHillClimb <- function(matrix, ...) {
  Permuted <- PermuteMatrix(matrix)
  max <- KingOfTheHill(Permuted, ...)
  max
}

## This is the significance test with null hypothesis being the same values for the matrix entries but in any random position, also
## has an effect size estimate of the hilliness.

permutationMeanDifferenceHilly <- function(matrix, repsStep1 = 1000, repsStep2 = 1000) {
  regularMatrixClimb <- replicate(repsStep1, KingOfTheHill(matrix))
  permutedMatrixClimb <- replicate(repsStep1, RandomHillClimb(matrix))
  permutationMeanDiff <- replicate(repsStep2, mean(sample(regularMatrixClimb, size = repsStep1, replace = T)) - mean(sample(permutedMatrixClimb, size = repsStep1, replace = T)))
  list(meanDiff = mean(permutationMeanDiff), pValueMeanDiffGreaterThan0 = ifelse((1-mean(permutationMeanDiff > 0)) > 0, (1-mean(permutationMeanDiff > 0)), paste0(1/repsStep2, " >")), fullResult = permutationMeanDiff)
}
