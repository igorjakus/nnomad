type optim_error = 
  | DivisionByZero 
  | NoConvergence 
  | NoRootInInterval
  | MaxIterationsReached
  | InvalidInput of string

let ( >>= ) = Result.bind
