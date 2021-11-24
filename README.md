# MetaGenyo
MetaGenyo is a R/Shiny app for Genetic Association Studies Meta-Analysis. The article is published in [BMC Bioinformatics](https://doi.org/10.1186/s12859-017-1990-4). The URL is https://metagenyo.genyo.es.

## Application organization
All the Shiny applications have the same basic structure:
* **global.R**: This script is ran once and it should load the necessary dependencies and contain the general functions used for all the sessions.
* **ui.R**: Here, the User Interface (UI) is constructed.
* **server.R**: Contains all the internal functions of the application.
* **www folder**: Here the files that appear in the application should be placed (e.g. the logos).
* **data folder**: Put here the internal data necessary to run the application (not applicable in this application).
 
 ## Deployment
 This tool is currently deployed on the virtual machine imageo.genyo.es (IP 192.168.2.35) of GENyO. THe installed SO in this machine is Ubuntu 18.04.4 LTS.
 
 ### Install dependencies
 #### R >= 3.6.3
```
sudo apt install apt-transport-https software-properties-common
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
sudo add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/'
sudo apt update
sudo apt install r-base
sudo apt install build-essential
sudo apt-get install libgdal-dev
```
 #### R packages
```
sudo R
install.packages(c('shiny','leaflet', 'DT', 'ggplot2', 'plotly', 'rgdal', 'plyr', 'shinycssloaders', 'shinythemes','leaflet.extras','leaflet.extras2', 'scales', 'ppcor'))
```
 #### Shiny Server
```
sudo apt-get install gdebi-core
# Check last version link at https://rstudio.com/products/shiny/download-server/ubuntu/
wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.13.944-amd64.deb
sudo gdebi shiny-server-1.5.13.944-amd64.deb
sudo chown -R shiny:shiny srv/shiny-server/
```
 #### Other dependencies
```
sudo apt-get install libssl-dev
```
 ### Install and configure Apache
 Installation:
 ```
 sudo apt-get install apache2 apache2-utils
sudo ufw allow 'Apache'
sudo a2enmod proxy
sudo a2enmod proxy_http
sudo a2enmod proxy_balancer
sudo a2enmod lbmethod_byrequests
```
Configuration:
```
sudo nano /etc/apache2/sites-available/000-default.conf
```
Copy this into <VirtualHost *:80>:
```
	ProxyPreserveHost On
	ProxyPass / http://covid19.genyo.es:3838/
	ProxyPassReverse / http://covid19.genyo.es:3838/
	ServerName covid19.genyo.es```
```
Save the file and restart:
```
sudo systemctl restart apache2
```
Switch off the directory listing for security reasons:
```
sudo nano /etc/apache2/apache2.conf
```
Change to:
```
<Directory /var/www/>
        Options FollowSymLinks
        AllowOverride None
        Require all granted
</Directory>
```
Save the file and reload:
```
sudo service apache2 reload
```
### Shiny-Server configuration
After creating a new account in Google Analytics (user bioinformatics.genyo@gmail.com) and getting the ID for the application:
```
sudo nano /etc/shiny-server/shiny-server.conf

# add this:     
google_analytics_id "UA-164531993-1";

directory_index off
```
Save the file and restart:
```
sudo systemctl restart shiny-server
```

### HTTPS configuration
Ask to the Informatics services to redirect the desired URL to this server and to open the port 80 in order to be accesible both grom GENyO and from outside. Then, use certbot to install a free certificate and redirect http to https:
```
sudo apt-get update
sudo apt-get install software-properties-common
sudo add-apt-repository universe
sudo add-apt-repository ppa:certbot/certbot
sudo apt-get update
sudo apt-get install certbot python-certbot-apache

sudo certbot --apache
```
### Clone the application
```
cd /srv/shiny-server
sudo git clone https://github.com/jordimartorell/DatAC2.git .

sudo chown -R shiny:shiny *
```

## Updates
Simply make the necessary changes in this repository and update it on the server:
```
cd /srv/shiny-server
git pull origin master
```

