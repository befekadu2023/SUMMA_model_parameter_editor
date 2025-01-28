# SUMMA_model_parameter_editor
This is a Fortran code developed to edit the SUMMA model parameters. It requires a text file named 'model.in'. The parameter names recognized by SUMMA should be concatenated with an underscore followed by a space and the intended value, an a single row should be used for each parameter that needs to be edited.
In addition, the two parameter files of SUMMA, namely, 'localParamInfo.txt' and 'basinParamInfo.txt', need to be copied and renamed as 'localParamInfo_default_sundials.txt' and 'basinParamInfo_default.txt', respectively, to avoid overriding the default parameter values. The parameter editor uses the two copied files to populate the parameter names and values  which are not provided in the 'model.in' file.

The compilation procedure on HPCs is as follows:


First, load the version of the intel module you would like to use, and then, run the following commands in the directory of your fortran files.

ifort -c summa_parameter_ident.f90 precmod.f90 stringmod.f90

ar rc summa_par_editor_lib.a precmod.o stringmod.o summa_parameter_ident.o

ifort summa_parameter_editor.f90 summa_par_editor_lib.a
