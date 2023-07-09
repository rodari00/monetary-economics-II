
using CSV
using DataFrames
using Plots
using StatsBase
using LinearAlgebra
using Printf
using Logging
using RollingFunctions
using Pipe

#---------------------------------------------------------------
# Setup parent and children directories
root = pwd()


data_dir = joinpath(root,"data")

mkdir(joinpath(root,"results"))
mkdir(joinpath(root,"results","data"))
mkdir(joinpath(root,"results","figures"))
mkdir(joinpath(root,"scripts"))
mkdir(joinpath(root,"scripts","temp"))

df_list = readdir(data_dir);
df_cstat = CSV.read(joinpath(data_dir,df_list[1]), DataFrame)
names_df_compustat = names(df_cstat)
df_nber = CSV.read(joinpath(data_dir,df_list[2]), DataFrame)
names_df_nber = names(df_nber)



a = @pipe df_nber |>
dropmissing(_) |>
transform(_, [:pay, :vadd] => (p,s)-> p/s => :ciao)
sort(_,[:sic, :year]) |>
groupby(_, :sic) |>
transform(_, [:vadd,:matcost] => (p,s)-> p/s => ciao)





transform(_,)
transform(df, :vadd => (v -> runmean(v, 3))=>:)