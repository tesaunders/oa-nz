prev_year <- as.numeric(format(Sys.Date(), "%Y"))-1

plot_colours <- c("bronze" = "#A65628",
                  "hybrid" = "#377EB8",
                  "gold" = "#FFFF33",
                  "diamond" = "lightgrey",
                  "green" = "#4DAF4A",
                  "closed" = "#E41A1C")

inst_lines_nz <- c("University of Auckland" = "#00467f",
                   "Auckland University of Technology" = "#000000",
                   "University of Waikato" = "#ffa400",
                   "Massey University" = "#8998b9",
                   "Victoria University of Wellington" = "#004b34",
                   "University of Canterbury" = "#ce0818",
                   "Lincoln University" = "#2d4ce5",
                   "University of Otago" = "#f2b043")

inst_lines_au <- c("The University of Melbourne" = "#000c38",
                   "Australian National University" = "#bf872b",
                   "The University of Sydney" = "#141414",
                   "The University of Queensland" = "#411d62",
                   "The University of Western Australia" = "#e2b600",
                   "The University of Adelaide" = "#005a9c",
                   "Monash University" = "#004b34",
                   "UNSW Sydney" = "#da4726")