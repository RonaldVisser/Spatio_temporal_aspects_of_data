#in case projection error use

Sys.setenv(PROJ_LIB="C:\\Users\\rvi04\\AppData\\Local\\R\\win-library\\4.3\\sf\\proj")
Sys.setenv(PROJ_LIB="C:\\Program Files\\PostgreSQL\\13\\share\\contrib\\postgis-3.1\\proj")

sf_proj_search_paths("C:\\Program Files\\PostgreSQL\\13\\share\\contrib\\postgis-3.1\\proj")


sf_proj_search_paths("C:\\Users\\rvi04\\AppData\\Local\\R\\win-library\\4.3\\sf\\proj")
