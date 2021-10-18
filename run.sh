#!/usr/bin/sh


mkdir data figures

cd data; wget https://github.com/uw-biomedical-ml/uwhvf/raw/master/alldata.json ; cd ..

./1-filter.py
./2-km-graph.R
./3-glm.R
./4-power-calc.R
