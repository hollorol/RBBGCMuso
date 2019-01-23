#!/bin/bash

DEBIAN_VERSION=$(cat /etc/debian_version)
if (( $(echo "$DEBIAN_VERSION < 8" | bc -l )))
then
    echo "Your Debian version is obsolated."
exit 1
fi
#INREPO=$(cat /etc/apt/sources.list|grep -c jessie-cran35)
if (( $(echo "$DEBIAN_VERSION < 9" | bc -l )))
then
    apt-get install apt-transport-https
    apt-key adv --keyserver keys.gnupg.net --recv-key 'E19F5F87128899B192B1A2C2AD5F960A256A04AF'
    echo "deb https://cloud.r-project.org/bin/linux/debian jessie-cran35/" >> /etc/apt/sources.list
    apt-get update
    apt-get -y upgrade
fi

#installing devtools's system dependency-s on Debian and Ubuntu
apt-get install --assume-yes build-essential libcurl4-gnutls-dev libxml2-dev libssl-dev r-base

#Installing the dependences

R --vanilla << EOF
packagesToInstall <- c("devtools", "magrittr",
                       "digest", "ggplot2",
                       "latex2epr", "tibble",
                       "tidyr", "limSolve", "rmarkdown")
install.packages(packagesToInstall,repos="https://cran.rstudio.com/")
EOF

#Installing RBBGCMuso

R --vanilla << EOF
devtools::install_github("hollorol/RBBGCMuso/RBBGCMuso")
EOF


