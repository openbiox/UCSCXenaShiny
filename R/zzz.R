.onAttach <- function(libname, pkgname) {
  version <- utils::packageDescription(pkgname, fields = "Version")
  
  msg <- paste0("=========================================================================================
", pkgname, " version ", version, "
Project URL: https://github.com/openbiox/UCSCXenaShiny
Usages: https://openbiox.github.io/UCSCXenaShiny/

If you use it in published research, please cite:
  Shixiang Wang, Yi Xiong, Longfei Zhao, Kai Gu, Yin Li, Fei Zhao, Jianfeng Li,
  Mingjie Wang, Haitao Wang, Ziyu Tao, Tao Wu, Yichao Zheng, Xuejun Li, Xue-Song Liu,
  UCSCXenaShiny: An R/CRAN Package for Interactive Analysis of UCSC Xena Data, 
  Bioinformatics, 2021;, btab561, https://doi.org/10.1093/bioinformatics/btab561.
=========================================================================================
                              --Enjoy it--")
  base::packageStartupMessage(msg)
}