# OSCER ####

# login
# ssh ouecon010@schooner.oscer.ou.edu

# move files to my fork w/ bash
# scp /c/Users/caleb/Downloads/PS2_Reid.tex /c/Users/caleb/Downloads/PS2_Reid.pdf ouecon010@schooner.oscer.ou.edu:~/DScourseS26/

# SQLite ####
df <- iris %>% as_tibble()

sqldf('SELECT count(*) FROM df WHERE Species = "virginica"')

# store the output:
counted <- sqldf('SELECT count(*) FROM df WHERE Species = "virginica"')

# dplyr
counted.dplyr <- df %>% filter(Species=="virginica") %>% count %>% print()

# check if results are same
identical(counted[[1]],counted.dplyr[[1]])

