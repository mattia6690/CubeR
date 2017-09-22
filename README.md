# CubeR
## An R-package to access data cubes via WCS and WCPS queries.


#### Download the Package from Gitlab

For now this Package is in a develpment stage and therefore privatly hosted in a Gitlab repository. It is available only to registered group members and the access has to be granted by one of the active members of the group (see [Members Section](https://gitlab.inf.unibz.it/REMSEN/CubeR/project_members) or below)
A general way to load the package in R is provided with the following code:<br>

```r
library(devtools)
library(git2r)
library(getPass)

uname<-     "Your Gitlab Username"
password<-  "Your Gitlab Passwort" # Manual insertion
password<-  getPass::getPass() # Password Popup


devtools::install_git("https://gitlab.inf.unibz.it/REMSEN/CubeR", 
  credentials = git2r::cred_user_pass(uname, password)
)

```

#### Contributors

* [Mattia Rossi] (https://gitlab.inf.unibz.it/Mattia.Rossi)
* [Daniel Frisinghelli] (https://gitlab.inf.unibz.it/Daniel.Frisinghelli)<br>

<br>![](http://www.eurac.edu/Style%20Library/logoEURAC.jpg)<br><br>

#### Contact

For further information please contact [Mattia Rossi](mattia.rossi@eurac.edu) or [Daniel Frisinghelli](daniel.frisinghelli@eurac.edu)
