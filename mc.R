mc <- data.frame(
  pool5 =    c(1473.79,  1461.6),
  pool10 =   c(1501.446, 1482.6),
  poolzoi5 = c(1670.809, 1655.2), 
  hier5 =    c(1735.366, 1645.4),
  cs5 =      c(136.7929, 123.0),
  gp5 =      c(177.1327, 146.7)
                    )
rownames(mc) <- c("ELPD.in", "ELPD.loo")

mc
# Improvement             # In      Loo   % improvement
#_______________________________________________________________
# pool 10 vs 5          
mc$pool10 - mc$pool5      # 27.656  21.000
((mc$pool10 - mc$pool5)/mc$pool5)*100     # 1.876522 1.436782

# zoi5 vs pool5
mc$poolzoi5 - mc$pool5    # 197.019 193.600
((mc$poolzoi5 - mc$pool5)/mc$pool5)*100   #13.36819 13.24576

# hier5 vs zoi
mc$hier5 - mc$poolzoi5       # 64.557 -9.800
((mc$hier5 - mc$poolzoi5 )/mc$poolzoi5)*100     # 3.8638169 -0.5920735

# gp5 vs cs5
mc$gp5 - mc$cs5           # 40.3398 23.7000
((mc$gp5 - mc$cs5)/mc$cs5)*100            # 29.48969 19.26829
 


# # hier5 vs pool5
# mc$hier5 - mc$pool5       # 261.576 183.800
# ((mc$hier5 - mc$pool5 )/mc$pool5)*100     # 17.74853 12.57526



