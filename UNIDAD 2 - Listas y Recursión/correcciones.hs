aplanar :: [[a]] -> [a]
aplanar []       = []
aplanar (ls:lss)   =  ls ++ aplanar lss 