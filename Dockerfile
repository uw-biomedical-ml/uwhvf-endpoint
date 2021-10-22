FROM r-base:4.1.1
RUN apt-get update 
RUN apt-get install --assume-yes git ssh-client sudo libxml2-dev libxslt-dev wget libcurl4-openssl-dev libssl-dev 

#Miniconda 
RUN wget https://repo.anaconda.com/miniconda/Miniconda3-py38_4.10.3-Linux-x86_64.sh 
RUN bash Miniconda3-py38_4.10.3-Linux-x86_64.sh -b
ENV PATH=/root/miniconda3/bin:${PATH} 

#Python Packages 
RUN conda install python=3.8.3
RUN pip install numpy tqdm

#R Packages
RUN R -e "install.packages('devtools',dependencies=TRUE, repos='http://cran.rstudio.com/')" 
RUN R -e "devtools::install_github('kassambara/survminer', build_vignettes = FALSE)"
RUN R -e "install.packages(c('glmnet','ggplot2','doMC','pwr'))"

WORKDIR /data
RUN git clone https://github.com/uw-biomedical-ml/uwhvf-endpoint.git /data/app
WORKDIR /data/app
CMD ["run.sh"]
ENTRYPOINT ["sh","run.sh"]