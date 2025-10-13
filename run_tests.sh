cd tests
for file in ./*; do 
  if [ -f "$file" ]; then 
    canon=$(readlink -f "$file")
    ../bin/compiler "$canon"
    if [ $? -eq 0 ]; then 
      echo -e "\033[1;32m$canon compiled\033[0m"
    else 
      echo -e "\033[1;31m$canon didn't compile\033[0m"
    fi
  fi
done

for file in ./*; do
  if [ -f "$file" ]; then
    canon=$(readlink -f "$file")
    ../bin/compiler -run "$canon"
    if [ $? -eq 0 ]; then
      echo -e "\033[1;32m$canon passed\033[0m"
    else
      echo -e "\033[1;31m$canon failed\033[0m"
    fi
  fi
done