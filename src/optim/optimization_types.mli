type optim_error = 
  | DivisionByZero 
  | NoConvergence 
  | NoRootInInterval
  | MaxIterationsReached
  | InvalidInput of string

val ( >>= ) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
