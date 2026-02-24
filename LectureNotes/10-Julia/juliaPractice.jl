using LinearAlgebra

function matrix_multiplication(X, Y)
  product = X * Y
  return product
end

function execute_mat_mul()
  x = rand(15,500);
  y = rand(500,1000);
  z = matrix_multiplication(x,y)
  println(size(z))
  return nothing
end

@show execute_mat_mul()