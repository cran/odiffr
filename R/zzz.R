# Package attach hook. Only informs: it never prompts or downloads.
.onAttach <- function(libname, pkgname) {
  if (!odiff_available()) {
    packageStartupMessage(
      "odiff binary not found. Install it with:\n",
      "  - R: odiffr::install_odiff() (no Node.js needed)\n",
      "  - npm: npm install -g odiff-bin\n",
      "  - Download: https://github.com/dmtrKovalenko/odiff/releases"
    )
  }
}
