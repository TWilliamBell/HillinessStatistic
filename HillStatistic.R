## Hilliness Statistic ##

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
    if (Row == NewRow & Col == NewCol) {
      return(matrix[Row, Col])
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

## This is the significance test

Hilly <- function(matrix, reps = 100000, RoundTo = 0.0001, ...) {
  RandomClimber <- replicate(reps, RandomHillClimb(matrix, ...))
  ExperiencedClimber <- replicate(reps, KingOfTheHill(matrix))
  Max <- max(matrix)
  ProportionOfSuccessesForActualMatrix <- mean(sum(Max == ExperiencedClimber)/reps)
  ProportionOfSuccessesForPermutedMatrix <- rep(NA_real_, 1/RoundTo)
  for (i in 0:as.integer((1/RoundTo)-1)) {
    ProportionOfSuccessesForPermutedMatrix[i+1] <- mean(Max == RandomClimber[((i*reps)%/%(1/RoundTo)):(((i+1)*reps)%/%(1/RoundTo))])
  }
  PValue <- mean(ProportionOfSuccessesForPermutedMatrix > ProportionOfSuccessesForActualMatrix)
  PValue
}
