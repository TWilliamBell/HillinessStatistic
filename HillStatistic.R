## Hilliness Statistic ##

## The test is more or less considering how good the function is at climbing the hill of your matrix. 
## If it is significantly better at climbing the hill enough to get to the top of your actual matrix 
## much more than permuted versions of your matrix, then your matrix is probably significantly hillier 
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
      if (Row == NewRow & Col == NewCol) {
        return(matrix[Row, Col])
      }
    }
    Row <- NewRow
    Col <- NewCol
  }
  return(matrix[Row, Col])
}

## Permutes the matrix structure
PermuteMatrix <- function(matrix) {
  Matrix <- matrix(sample(matrix, length(matrix), replace = F), nrow = nrow(matrix))
}

## Function to be replicated

RandomHillClimb <- function(matrix, ...) {
  Permuted <- PermuteMatrix(matrix)
  max <- KingOfTheHill(Permuted, ...)
  max
}

## This is the significance test with null hypothesis being the same values for the matrix entries but in any random position

Hilly <- function(matrix, reps = 100000, RoundTo = 0.0001, ...) {
  RandomClimber <- replicate(reps, RandomHillClimb(matrix, ...))
  ExperiencedClimber <- replicate(as.integer(1/RoundTo), KingOfTheHill(matrix, ...))
  Max <- max(matrix)
  ProportionOfSuccessesForActualMatrix <- mean(Max == ExperiencedClimber)
  ProportionOfSuccessesForPermutedMatrix <- rep(NA_real_, as.integer(1/RoundTo))
  for (i in 0:as.integer((1/RoundTo)-1)) {
    ProportionOfSuccessesForPermutedMatrix[i+1] <- mean(Max == RandomClimber[((i*reps)%/%(1/RoundTo)):(((i+1)*reps)%/%(1/RoundTo))])
  }
  PValue <- mean(ProportionOfSuccessesForPermutedMatrix > ProportionOfSuccessesForActualMatrix)
  PValue
}
