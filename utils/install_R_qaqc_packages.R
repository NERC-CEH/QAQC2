pkgs = c(
  "rcolors",
  "khroma",
  "scico",
  "worldmet",
  "rmdformats"
)

repos = "https://cloud.r-project.org"

to_install = pkgs[!pkgs %in% rownames(installed.packages())]

if (length(to_install) > 0) {
  install.packages(to_install, repos = repos)
} else {
  message("All packages already installed.")
}