## Hilliness Statistic ##

## A basic hill climbing algorithm
KingOfTheHill <- function(matrix, GradualSlope = F) { ## Doesn't work for sparse matrices, or 2x2 matrices
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
  warning("Hill climbing algorithm timed out, invalid input?")
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

## SignificanceTest

Hilly <- function(matrix, reps = 1000, ...) {
  ExperiencedClimber <- replicate(reps, RandomHillClimb(matrix, ...))
  Max <- max(matrix)
  SignificanceLevel <- 1-sum(Max > ExperiencedClimber)/reps
  SignificanceLevel
}
