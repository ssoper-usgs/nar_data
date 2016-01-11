Linux: [![travis](https://api.travis-ci.org/USGS-CIDA/nar_data.svg?branch=master)](https://travis-ci.org/USGS-CIDA/nar_data/)

Windows: [![Windows Build Status](https://ci.appveyor.com/api/projects/status/6kk173okrw3j0ibb/branch/master?svg=true)](https://ci.appveyor.com/project/cschroed-usgs/nar-data-wyols/branch/master)

This project requres the git large file storage extension. Before you clone, Install git lfs here:

https://git-lfs.github.com/

When retrieving large files from the server (ex: cloning, pulling), you will be prompted for credentials. You can supply a blank username and password. You only need to supply credentials if you wish to share modified large files.

To perform read-only checkouts of this project without being prompted for credentials run:

```
git config --global credential.helper 'store --file .anonymous-git-credentials'
```

