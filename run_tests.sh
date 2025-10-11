cd tests


for file in ./*; do
  ../bin/debug/bindings "$file"
  if [ $? -eq 0 ]; then
    echo -e "\033[1;32m$file passed\033[0m"
  fi
done