# cleans up logging output
grep '\--->' | sed 's@ --->@@g' | awk '$8 == "PRIVMSG" { sub("!.*", ":", $7); printf "%s %s %s %s %s %s %-17s ", $1, $2, $3, $4, $5, $6, $7; for (i=10;i<=NF;i++) printf "%s ", $i;printf "\n" } ' 
