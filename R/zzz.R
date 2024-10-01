.onAttach <- function(libname, pkgname) {
  version <- utils::packageDescription(pkgname, fields = "Version")

  msg <- paste0("=========================================================================================
", pkgname, " version ", version, "
Project URL: https://github.com/openbiox/UCSCXenaShiny
Usages: https://openbiox.github.io/UCSCXenaShiny/

If you use it in published research, please cite:
  Shensuo Li, Yuzhong Peng, Minjun Chen, Yankun Zhao, Yi Xiong, Jianfeng Li, Peng Luo, 
  Haitao Wang, Fei Zhao, Qi Zhao, Yanru Cui, Sujun Chen, Jian-Guo Zhou, Shixiang Wang,  
  Facilitating integrative and personalized oncology omics analysis with UCSCXenaShiny, 
  Communications Biology, 1200 (2024),  https://doi.org/10.1038/s42003-024-06891-2
=========================================================================================
                              --Enjoy it--")
  base::packageStartupMessage(msg)
}



