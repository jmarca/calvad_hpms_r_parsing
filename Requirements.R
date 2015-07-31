## need node_modules directories
dot_is <- getwd()
node_paths <- dir(dot_is,pattern='\\.Rlibs',
                  full.names=TRUE,recursive=TRUE,
                  ignore.case=TRUE,include.dirs=TRUE,
                  all.files = TRUE)
path <- normalizePath(node_paths, winslash = "/", mustWork = FALSE)
lib_paths <- .libPaths()
.libPaths(c(path, lib_paths))

## ideally I would plumb versions from package.json environment variables?

envrr <- Sys.getenv()
print(envrr)
dependencies <- grep(pattern='npm_package_rDependencies'
                    ,x=names(envrr),perl=TRUE,value=TRUE)
print(dependencies)
pkgs <- strsplit(x=dependencies,split='npm_package_rDependencies_')
print(pkgs)
for(i in 1:length(dependencies)){
    pkg <- pkgs[[i]][2]
    ver <- envrr[[dependencies[i]]]
    vc <-  list(op=">=",version=package_version(ver))
    print(vc)
    if(!requireNamespace(package=pkg,versionCheck=vc)){
        print('need to download')
        devtools::install_github(paste('hadley',pkg,sep='/'))
        ## whoops, need to add proper github user, repo name here
    }else{
        print(paste('got',pkg,ver,'already'))
    }
}
