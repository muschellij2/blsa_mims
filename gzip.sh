for i in *RAW.csv *.gt3x; 
do
    echo "${i}";
    gzip -9 "${i}";
done
