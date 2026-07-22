
gmake clean
gmake 
./test_adjoint < input
./adjoint_driver <input
./hessian_driver <input_hess

gmake clean
cp eco_params_full.f90 eco_params.f90
gmake 
./test_adjoint <input_hess
gmake clean 
