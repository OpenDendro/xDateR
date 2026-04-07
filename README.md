# xDateR

## Overview

A shiny app for interactive crossdating. This is deployed as part of openDendro at:

https://viz.datascience.arizona.edu/xDateR/ 

## Package Management and Deployment Notes for Devs
This app is deployed on a Posit Connect server at the University of Arizona. Package management for deployment is handled via manifest.json, which must be generated on a Linux machine running the same version of R as the server.
Do not regenerate manifest.json locally on a Mac or Windows machine and commit it to the repo. Doing so will embed platform-specific binary information that is incompatible with the Linux server, causing the deployment to fail.
If packages need to be added, updated, or removed:
Update the package dependencies in the app code
Run renv::snapshot() to update renv.lock
Contact the server admin to regenerate manifest.json on the server
Commit and push both renv.lock and the new manifest.json

The renv.lock file is platform-neutral and should be kept in sync with manifest.json. It is used for local development reproducibility — run renv::restore() to recreate the package environment locally.