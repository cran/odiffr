# Package attach hook
.onAttach <- function(libname, pkgname) {
  if (!odiff_available()) {
    packageStartupMessage(
      "odiff binary not found. Install via:\n",
      "  - npm: npm install -g odiff-bin\n",
      "  - Download: https://github.com/dmtrKovalenko/odiff/releases\n",
      "  - R: odiffr_update()"
    )
  }
}
