clear

cd "C:\Users\9335556.FFLCH\Downloads\Trabalho final"

import delimited "C:\Users\9335556.FFLCH\Downloads\Trabalho final\base_cadastro.csv", delimiter(";") varnames(1) clear

save base_cadastro.dta, replace

import delimited "C:\Users\9335556.FFLCH\Downloads\Trabalho final\PERFIL_SERVIDOR_2016_FINAL.csv", delimiter(";") varnames(1) clear

rename dc_unidade_base nomesc

keep if dc_cargo_base == "DIRETOR DE ESCOLA"

save servidores.dta, replace

merge m:m nomesc using base_cadastro.dta, keep(match)

save dir_esc.dta 

