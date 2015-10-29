money <- c(`Mortgage ($84,911)`=84911, `Auto andntuition loans ($14,414)`=14414, 
             `Home equity loans ($10,062)`=10062, `Credit Cards ($8,565)`=8565,`Savings each year ($392)`=392)
waffle(money/392, rows=7, size=0.5, 
       colors=c("#c7d4b6", "#a3aabd", "#a0d0de", "#97b5cf"), 
       title="Average Household Savings Each Year", 
       xlab="1 square == $392")

poult <- c(`Eggs (96606)`=96606,`Chicks/pullets (22993)`=22993, `mature hen(12235)`=12235, 
             `mature cock (6367)`=6367)
waffle(poult/1000, rows=7, size=0.5, 
       colors=c("#c7d4b6", "#a3aabd", "#a0d0de", "#97b5cf"), 
       title="Total Poultry products Tharaka Nithi County", 
       xlab="1 square == 1000")
