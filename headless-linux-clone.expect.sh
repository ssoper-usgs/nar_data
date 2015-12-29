#!/usr/bin/expect

#This should successfully enter blank usernames and passwords when git lfs prompts for them
spawn git clone git@github.com:USGS-CIDA/nar_data.git
expect "Username for 'https://cida-test.er.usgs.gov': "
send "\r";
expect "Password for 'https://cida-test.er.usgs.gov': "
send "\r";
wait
