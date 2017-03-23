# **NEWS**

***
## **plenar 0.2.0**

#### *New Features/Functions*

* [available_data](R/available_data.R) - A function to query the meta-data on the API with enabled filtering on name, date and GeoJSON polygon regions. Now returns a data.table instead of a nested list.


## **plenar 0.1.0**

#### *New Features/Functions*

* [validate_url](R/validate_url.R) - A function to do some basic sanity checks on URLs. Meant to be used as an intermediary to sanitize urls before executing calls to the API
* [get_response](R/get_response.R) - A function to automate the checking of response codes and extracting of content
