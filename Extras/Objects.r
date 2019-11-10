library(readr)

dir_create("BRI_Shiny/Images")


dir_create("BRI_Shiny/r_objects")

write_rds(data7, path = "BRI_Shiny/r_objects/data7.rds")
write_rds(linearMod10, path = "BRI_Shiny/r_objects/linearMod10.rds")
write_rds(linearMod14, path = "BRI_Shiny/r_objects/linearMod14.rds")
write_rds(data11, path = "BRI_Shiny/r_objects/data11.rds")
